use core::cell::Cell;
use std::collections::BTreeSet;

use shared::{LogicalUnit, Point, Rect, Size};

use crate::{
    SystemLink, WindowHandle, WindowRootView,
    input::hittest::{HitTestTreeData, HitTestTreeRef},
    rendering::composite::{CompositeRect, CompositeTreeRef},
    ui::dock::tab::{
        PaneGroupTabStripView, PaneGroupTabStripViewInit, PaneGroupTabView, PaneGroupTabViewInit,
    },
    uicore::{
        DeriveTeardownContext, MeasureContext, RenderContext, SystemLinkAccess, TeardownContext,
        TypedViewIdentifier, View, ViewConstructor, ViewDestructionContext, ViewIdentifier,
        ViewImmediateTeardownable, ViewInitContext, ViewInstanceQueryable,
        ViewInstanceQueryableMut, ViewInstanceStore, ViewLayout, ViewLayoutStateStore,
        ViewRegisterable, ViewRelationControllable, ViewRelationQueryable, ViewRenderElements,
        ViewRenderQueue, ViewRenderer, ViewSize, ViewTreeRelationStore,
    },
};

/// デザイン定数
struct DesignMetrics {
    /// Splitterの太さ
    splitter_thickness: f32,
    /// タブの余白(X方向)
    tab_padding_x: f32,
    /// タブの余白(Y方向)
    tab_padding_y: f32,
    /// タブの内容(表示テキスト)の高さ
    tab_content_height: f32,
    /// タブの角丸
    tab_rounding: f32,
}
impl DesignMetrics {
    /// タブの高さを計算する
    const fn tab_height(&self) -> f32 {
        self.tab_content_height + self.tab_padding_y * 2.0
    }
}

/// デザイン定数
const DESIGN_METRICS: DesignMetrics = DesignMetrics {
    splitter_thickness: 4.0,
    tab_padding_x: 8.0,
    tab_padding_y: 4.0,
    tab_content_height: 16.0,
    tab_rounding: 8.0,
};

pub mod splitter;
mod tab;

/// Paneの表示内容
pub trait PaneContentPresenter {
    /// Pane ID(復帰時の識別につかわれる)
    fn id(&self) -> String;
    /// タブ名
    fn name(&self) -> String;
    /// ルートとなるViewのID
    fn root_view_id(&self) -> ViewIdentifier;

    /// 後始末
    #[allow(unused_variables)]
    #[inline(always)]
    fn teardown(&mut self, ctx: &mut TeardownContext) {}

    /// サイズ変更
    #[allow(unused_variables)]
    #[inline(always)]
    fn resize(&self, new_size: &Size<LogicalUnit>, context: &mut PaneContentResizeContext) {}
}

pub struct PaneContentResizeContext<'env> {
    pub view_instance_store: &'env mut ViewInstanceStore,
    pub view_render_queue: &'env mut ViewRenderQueue,
    pub view_tree_relation_store: &'env ViewTreeRelationStore,
}
impl ViewInstanceQueryable for PaneContentResizeContext<'_> {
    #[inline(always)]
    fn view_instance_of<T: View + 'static>(&self, id: ViewIdentifier) -> Option<&T> {
        crate::uicore::view_instance(id, self.view_instance_store)
    }

    #[inline(always)]
    fn view_layout_untyped(&self, id: ViewIdentifier) -> Option<&ViewLayout> {
        crate::uicore::view_layout(id, self.view_instance_store)
    }
}
impl ViewInstanceQueryableMut for PaneContentResizeContext<'_> {
    #[inline(always)]
    fn view_instance_mut_of<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T> {
        crate::uicore::view_instance_mut(id, self.view_instance_store)
    }

    #[inline(always)]
    fn view_set_visibility_untyped(&mut self, id: ViewIdentifier, visible: bool) {
        crate::uicore::view_set_visibility(id, visible, self.view_instance_store);
    }

    #[inline(always)]
    fn view_layout_mut_untyped(&mut self, id: ViewIdentifier) -> Option<&mut ViewLayout> {
        crate::uicore::view_layout_mut(id, self.view_instance_store)
    }
}
impl ViewRenderer for PaneContentResizeContext<'_> {
    #[inline(always)]
    fn schedule_view_render_untyped(&mut self, target: ViewIdentifier) {
        self.view_render_queue.schedule(target);
    }
}
impl ViewRelationQueryable for PaneContentResizeContext<'_> {
    #[inline(always)]
    fn view_get_parent_untyped(&self, id: ViewIdentifier) -> Option<ViewIdentifier> {
        crate::uicore::view_get_parent(id, self.view_tree_relation_store)
    }
}

pub trait DerivePaneContentResizeContext {
    fn derive_pane_content_resize_context<'env2>(
        &'env2 mut self,
    ) -> PaneContentResizeContext<'env2>;
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DockID(u32);
impl DockID {
    #[inline(always)]
    fn from_store_index(v: usize) -> Self {
        Self(v.try_into().expect("too many docks!"))
    }

    #[inline(always)]
    const fn store_index(&self) -> usize {
        self.0 as _
    }
}

pub struct DockStore {
    docks: Vec<Option<Dock>>,
    computed_states: Vec<ComputedDockState>,
    free_id: BTreeSet<DockID>,
}
impl DockStore {
    pub fn new() -> Self {
        Self {
            docks: Vec::new(),
            computed_states: Vec::new(),
            free_id: BTreeSet::new(),
        }
    }

    pub fn alloc(&mut self, dock: impl FnOnce(DockID) -> Dock) -> DockID {
        if let Some(id) = self.free_id.pop_first() {
            self.docks[id.store_index()] = Some(dock(id));
            return id;
        }

        let id = DockID::from_store_index(self.docks.len());
        self.docks.push(Some(dock(id)));
        self.computed_states.push(ComputedDockState {
            rect: Rect::from_lt_size(Point::new_logical(0.0, 0.0), Size::new_logical(0.0, 0.0)),
        });
        id
    }

    pub fn alloc_recurse(&mut self, dock: impl FnOnce(DockID, &mut Self) -> Dock) -> DockID {
        if let Some(id) = self.free_id.pop_first() {
            let dock = dock(id, self);
            self.docks[id.store_index()] = Some(dock);
            return id;
        }

        let id = DockID::from_store_index(self.docks.len());
        self.docks.push(None);
        self.computed_states.push(ComputedDockState {
            rect: Rect::from_lt_size(Point::new_logical(0.0, 0.0), Size::new_logical(0.0, 0.0)),
        });

        let dock = dock(id, self);
        self.docks[id.store_index()] = Some(dock);

        id
    }

    fn free(&mut self, id: DockID) -> Dock {
        if id.store_index() == self.docks.len() + 1 {
            // tail freed
            self.computed_states.pop();
            return self
                .docks
                .pop()
                .expect("returned to empty")
                .expect("already freed?");
        }

        self.free_id.insert(id);
        self.docks[id.store_index()].take().expect("already freed?")
    }

    fn replace(&mut self, id: DockID, new_dock: Dock) -> Dock {
        core::mem::replace(
            self.docks[id.store_index()]
                .as_mut()
                .expect("already freed?"),
            new_dock,
        )
    }

    fn get(&self, id: DockID) -> &Dock {
        self.docks[id.store_index()]
            .as_ref()
            .expect("already freed?")
    }

    fn get_mut(&mut self, id: DockID) -> &mut Dock {
        self.docks[id.store_index()]
            .as_mut()
            .expect("already freed?")
    }

    fn get_computed_state(&self, id: DockID) -> &ComputedDockState {
        &self.computed_states[id.store_index()]
    }

    fn get_computed_state_mut(&mut self, id: DockID) -> &mut ComputedDockState {
        &mut self.computed_states[id.store_index()]
    }

    pub fn alloc_root(&mut self, content: impl FnOnce(DockID, &mut Self) -> DockID) -> DockID {
        self.alloc_recurse(move |root_id, store| Dock::RootContainer {
            content: content(root_id, store),
        })
    }

    pub fn alloc_fill(
        &mut self,
        root_view: TypedViewIdentifier<WindowDockRootView>,
        parent: DockID,
        init_ctx: &mut PaneGroupCreateContext,
        contents: impl FnOnce(&mut ViewInitContext) -> Vec<Box<dyn PaneContentPresenter>>,
        initial_active_index: usize,
    ) -> DockID {
        self.alloc(move |id| {
            let contents = contents(init_ctx.view_init_context);

            Dock::Fill {
                group_view_controller: PaneGroupViewController::new(
                    init_ctx,
                    root_view,
                    contents,
                    id,
                    initial_active_index,
                ),
                parent,
            }
        })
    }

    #[tracing::instrument(skip(self, root))]
    fn dump(&self, root: DockID) {
        fn rec(store: &DockStore, id: DockID, level: usize, sink: &mut String) {
            use core::fmt::Write;

            if !matches!(
                store.docks[id.store_index()],
                Some(Dock::RootContainer { .. }),
            ) {
                sink.extend(core::iter::repeat_n(' ', level * 2));
                writeln!(sink, "#{} {:?}", id.0, store.docks[id.store_index()]).unwrap();
            }

            match store.docks[id.store_index()] {
                Some(Dock::RootContainer { content }) => {
                    rec(store, content, level, sink);
                }
                Some(Dock::Fill { .. }) => {}
                Some(Dock::Splitted { docked, rest, .. }) => {
                    rec(store, docked, level + 1, sink);
                    rec(store, rest, level + 1, sink);
                }
                None => {}
            }
        }

        let mut sink = String::new();
        rec(self, root, 0, &mut sink);
        tracing::debug!("{sink}");
    }
}

struct ComputedDockState {
    rect: Rect<LogicalUnit>,
}

pub enum DockDirection {
    ToLeft(Cell<f32>),
    ToRight(Cell<f32>),
    ToTop(Cell<f32>),
    ToBottom(Cell<f32>),
}
impl DockDirection {
    const fn splitter_direction(&self) -> splitter::Direction {
        match self {
            Self::ToLeft(_) | Self::ToRight(_) => splitter::Direction::Horizontal,
            Self::ToTop(_) | Self::ToBottom(_) => splitter::Direction::Vertical,
        }
    }

    /// return: (docked, rest, splitter)
    fn split_rect(
        &self,
        full: &Rect<LogicalUnit>,
    ) -> (Rect<LogicalUnit>, Rect<LogicalUnit>, Rect<LogicalUnit>) {
        match self {
            Self::ToLeft(w) => {
                let width = w.get();
                let l_rect = full.slice_left(width);
                let r_rect =
                    full.slice_right(full.width - width - DESIGN_METRICS.splitter_thickness);
                let s_rect = Rect::from_lt_size(
                    Point::new_logical(l_rect.right(), full.top),
                    Size::new_logical(DESIGN_METRICS.splitter_thickness, full.height),
                );

                (l_rect, r_rect, s_rect)
            }
            Self::ToRight(w) => {
                let width = w.get();
                let l_rect =
                    full.slice_left(full.width - width - DESIGN_METRICS.splitter_thickness);
                let r_rect = full.slice_right(width);
                let s_rect = Rect::from_lt_size(
                    Point::new_logical(l_rect.right(), full.top),
                    Size::new_logical(DESIGN_METRICS.splitter_thickness, full.height),
                );

                (r_rect, l_rect, s_rect)
            }
            Self::ToTop(h) => {
                let height = h.get();
                let t_rect = full.slice_top(height);
                let b_rect =
                    full.slice_bottom(full.height - height - DESIGN_METRICS.splitter_thickness);
                let s_rect = Rect::from_lt_size(
                    Point::new_logical(full.left, t_rect.bottom()),
                    Size::new_logical(full.width, DESIGN_METRICS.splitter_thickness),
                );

                (t_rect, b_rect, s_rect)
            }
            Self::ToBottom(h) => {
                let height = h.get();
                let t_rect =
                    full.slice_top(full.height - height - DESIGN_METRICS.splitter_thickness);
                let b_rect = full.slice_bottom(height);
                let s_rect = Rect::from_lt_size(
                    Point::new_logical(full.left, t_rect.bottom()),
                    Size::new_logical(full.width, DESIGN_METRICS.splitter_thickness),
                );

                (b_rect, t_rect, s_rect)
            }
        }
    }
}

/// ドック形状
pub enum Dock {
    /// 最上位コンテナ（ウィンドウごとにひとつ）
    RootContainer { content: DockID },
    Fill {
        parent: DockID,
        group_view_controller: PaneGroupViewController,
    },
    Splitted {
        parent: DockID,
        docked: DockID,
        rest: DockID,
        splitter: TypedViewIdentifier<splitter::View>,
        direction: DockDirection,
    },
}
impl core::fmt::Debug for Dock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RootContainer { content } => f
                .debug_struct("Dock::RootContainer")
                .field("content", content)
                .finish_non_exhaustive(),
            Self::Fill { parent, .. } => f
                .debug_struct("Dock::Fill")
                .field("parent", parent)
                .finish_non_exhaustive(),
            Self::Splitted {
                parent,
                docked,
                rest,
                ..
            } => f
                .debug_struct("Dock::Splitted")
                .field("parent", parent)
                .field("docked", docked)
                .field("rest", rest)
                .finish_non_exhaustive(),
        }
    }
}
impl Dock {
    fn destruct(
        self,
        env: &mut (impl ViewRegisterable + ViewDestructionContext + DeriveTeardownContext + ?Sized),
    ) {
        match self {
            Self::RootContainer { .. } => {}
            Self::Fill {
                group_view_controller,
                ..
            } => {
                group_view_controller.destruct(env);
            }
            Self::Splitted { splitter, .. } => {
                env.destruct_view_recursive(splitter);
            }
        }
    }

    const fn parent(&self) -> Option<DockID> {
        match self {
            &Self::RootContainer { .. } => None,
            &Self::Fill { parent, .. } => Some(parent),
            &Self::Splitted { parent, .. } => Some(parent),
        }
    }

    fn maintain_dock_id_relation(
        &mut self,
        id: DockID,
        env: &mut (impl ViewInstanceQueryableMut + ?Sized),
    ) {
        match self {
            Self::RootContainer { .. } => {}
            Self::Fill {
                group_view_controller,
                ..
            } => group_view_controller.rebind_dock(id, env),
            &mut Self::Splitted { splitter, .. } => env
                .view_instance_mut(splitter)
                .expect("query failed")
                .rebind_controlling_dock(id),
        }
    }

    fn reparent(&mut self, new_parent: DockID) {
        match self {
            Self::RootContainer { .. } => unreachable!("reparenting root container"),
            Self::Fill { parent, .. } | Self::Splitted { parent, .. } => {
                *parent = new_parent;
            }
        }
    }

    fn replace_matching_child(&mut self, target: DockID, new: DockID) {
        match self {
            Self::RootContainer { content } if *content == target => {
                *content = new;
            }
            Self::Fill { .. } => unreachable!("fill cannot be nested"),
            Self::Splitted { docked, rest, .. } if *docked == target => {
                *docked = new;
            }
            Self::Splitted { docked, rest, .. } if *rest == target => {
                *rest = new;
            }
            t => unreachable!("invalid structure {t:?}"),
        }
    }
}

#[derive(Debug)]
pub enum DockingOperation {
    Merge(DockID),
    MergeAtTabIndex(DockID, usize),
    SplitToLeft(DockID),
    SplitToRight(DockID),
    SplitToTop(DockID),
    SplitToBottom(DockID),
    Diverge,
}

/// Dockを外した結果どうなったか？
#[derive(Clone, Copy, Debug)]
pub enum UndockResult {
    /// 普通に成功
    Success,
    /// Dockがすべてなくなる
    ToBeEmpty,
}

pub struct RedockingContext<'a, 'sys> {
    pub view_init_ctx: ViewInitContext<'a, 'sys>,
    pub view_render_queue: &'a mut ViewRenderQueue,
}
impl ViewRegisterable for RedockingContext<'_, '_> {
    #[inline(always)]
    fn construct_view_direct<T: View + 'static>(
        &mut self,
        ctor: impl FnOnce(TypedViewIdentifier<T>) -> Box<T>,
    ) -> TypedViewIdentifier<T> {
        self.view_init_ctx.construct_view_direct(ctor)
    }

    #[inline(always)]
    fn free_view_untyped(&mut self, id: ViewIdentifier) {
        self.view_init_ctx.free_view_untyped(id)
    }
}
impl ViewRelationControllable for RedockingContext<'_, '_> {
    #[inline(always)]
    fn view_set_parent_untyped(&mut self, id: ViewIdentifier, parent: ViewIdentifier) {
        crate::uicore::view_set_parent(id, parent, self.view_init_ctx.view_tree_relation_store)
    }

    #[inline(always)]
    fn view_detach_parent_untyped(&mut self, id: ViewIdentifier) {
        crate::uicore::view_detach_parent(id, self.view_init_ctx.view_tree_relation_store)
    }
}
impl ViewRenderer for RedockingContext<'_, '_> {
    #[inline(always)]
    fn schedule_view_render_untyped(&mut self, target: ViewIdentifier) {
        self.view_render_queue.schedule(target);
    }
}
impl ViewImmediateTeardownable for RedockingContext<'_, '_> {
    #[inline(always)]
    fn teardown_view_recursive_untyped(&mut self, target: ViewIdentifier) {
        crate::uicore::teardown_view_recursive(
            target,
            &mut TeardownContext {
                composite_tree: self.view_init_ctx.composite_tree,
                ht_manager: self.view_init_ctx.ht_manager,
                keyboard_focus_registry: self.view_init_ctx.keyboard_focus_registry,
                current_sec: self.view_init_ctx.current_sec,
                view_feedback_subscription_delayed_ops: self
                    .view_init_ctx
                    .view_feedback_subscription_delayed_ops,
            },
            self.view_init_ctx.view_instance_store,
            self.view_init_ctx.view_tree_relation_store,
            self.view_init_ctx.view_render_state_store,
        );
    }
}
impl ViewDestructionContext for RedockingContext<'_, '_> {
    #[inline(always)]
    fn destruct_view_recursive_untyped(&mut self, target: ViewIdentifier) {
        crate::uicore::destruct_view_recursive(
            target,
            &mut TeardownContext {
                composite_tree: self.view_init_ctx.composite_tree,
                ht_manager: self.view_init_ctx.ht_manager,
                keyboard_focus_registry: self.view_init_ctx.keyboard_focus_registry,
                current_sec: self.view_init_ctx.current_sec,
                view_feedback_subscription_delayed_ops: self
                    .view_init_ctx
                    .view_feedback_subscription_delayed_ops,
            },
            self.view_init_ctx.view_allocator,
            self.view_init_ctx.view_instance_store,
            self.view_init_ctx.view_tree_relation_store,
            self.view_init_ctx.view_group_relation_store,
            self.view_init_ctx.view_layout_state_store,
            self.view_init_ctx.view_render_state_store,
        );
    }
}
impl ViewInstanceQueryable for RedockingContext<'_, '_> {
    #[inline(always)]
    fn view_instance_of<T: View + 'static>(&self, id: ViewIdentifier) -> Option<&T> {
        crate::uicore::view_instance(id, self.view_init_ctx.view_instance_store)
    }

    #[inline(always)]
    fn view_layout_untyped(&self, id: ViewIdentifier) -> Option<&ViewLayout> {
        crate::uicore::view_layout(id, self.view_init_ctx.view_instance_store)
    }
}
impl ViewInstanceQueryableMut for RedockingContext<'_, '_> {
    #[inline(always)]
    fn view_instance_mut_of<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T> {
        crate::uicore::view_instance_mut(id, self.view_init_ctx.view_instance_store)
    }

    #[inline(always)]
    fn view_set_visibility_untyped(&mut self, id: ViewIdentifier, visible: bool) {
        crate::uicore::view_set_visibility(id, visible, self.view_init_ctx.view_instance_store);
    }

    #[inline(always)]
    fn view_layout_mut_untyped(&mut self, id: ViewIdentifier) -> Option<&mut ViewLayout> {
        crate::uicore::view_layout_mut(id, self.view_init_ctx.view_instance_store)
    }
}
impl DerivePaneContentResizeContext for RedockingContext<'_, '_> {
    fn derive_pane_content_resize_context<'env2>(
        &'env2 mut self,
    ) -> PaneContentResizeContext<'env2> {
        PaneContentResizeContext {
            view_instance_store: self.view_init_ctx.view_instance_store,
            view_render_queue: self.view_render_queue,
            view_tree_relation_store: self.view_init_ctx.view_tree_relation_store,
        }
    }
}
impl DeriveTeardownContext for RedockingContext<'_, '_> {
    fn derive_teardown_context<'env>(&'env mut self) -> TeardownContext<'env> {
        TeardownContext {
            composite_tree: self.view_init_ctx.composite_tree,
            ht_manager: self.view_init_ctx.ht_manager,
            keyboard_focus_registry: self.view_init_ctx.keyboard_focus_registry,
            current_sec: self.view_init_ctx.current_sec,
            view_feedback_subscription_delayed_ops: self
                .view_init_ctx
                .view_feedback_subscription_delayed_ops,
        }
    }
}
impl<'sys> SystemLinkAccess<'sys> for RedockingContext<'_, 'sys> {
    #[inline(always)]
    fn system_link<'a>(&'a self) -> &'a SystemLink<'sys> {
        self.view_init_ctx.system_link
    }
}

pub struct WindowDockRootView {
    window: WindowHandle,
}
impl View for WindowDockRootView {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        _ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        ViewRenderElements::EMPTY
    }

    fn teardown(&mut self, _ctx: &mut TeardownContext) {}

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }
}

pub struct WindowDockingManager {
    root_id: DockID,
    root_view_id: TypedViewIdentifier<WindowDockRootView>,
}
impl WindowDockingManager {
    #[tracing::instrument(skip(
        bound_window,
        window_root_view,
        ctx,
        view_render_queue,
        store,
        dock_ctor
    ))]
    pub fn new(
        bound_window: WindowHandle,
        window_root_view: TypedViewIdentifier<WindowRootView>,
        ctx: &mut ViewInitContext,
        view_render_queue: &mut ViewRenderQueue,
        max_rect: Rect<LogicalUnit>,
        store: &mut DockStore,
        dock_ctor: impl FnOnce(
            TypedViewIdentifier<WindowDockRootView>,
            &mut ViewInitContext,
            &mut ViewRenderQueue,
            &mut DockStore,
        ) -> DockID,
    ) -> Self {
        let root_view_id = ctx.construct_view_direct(|_| {
            Box::new(WindowDockRootView {
                window: bound_window,
            })
        });
        let l = ctx.view_layout_mut(root_view_id).expect("query failed");
        l.left_offset = max_rect.left;
        l.top_offset = max_rect.top;
        l.width = ViewSize::Fixed(max_rect.width);
        l.height = ViewSize::Fixed(max_rect.height);
        ctx.view_set_parent(root_view_id, window_root_view);

        let root_id = dock_ctor(root_view_id, ctx, view_render_queue, store);
        relayout_dock(
            root_id,
            store,
            Rect::from_lt_size(Point::new_logical(0.0, 0.0), max_rect.size()),
            &mut PaneContentResizeContext {
                view_instance_store: ctx.view_instance_store,
                view_render_queue,
                view_tree_relation_store: ctx.view_tree_relation_store,
            },
        );

        Self {
            root_id,
            root_view_id,
        }
    }

    pub fn teardown(self, store: &mut DockStore, env: &mut (impl ViewDestructionContext + ?Sized)) {
        // TODO: teardown docks
        env.destruct_view_recursive(self.root_view_id);
    }

    #[inline(always)]
    pub fn resize(
        &self,
        new_rect: Rect<LogicalUnit>,
        store: &mut DockStore,
        context: &mut PaneContentResizeContext,
    ) {
        let l = context
            .view_layout_mut(self.root_view_id)
            .expect("query failed");
        l.left_offset = new_rect.left;
        l.top_offset = new_rect.top;
        l.width = ViewSize::Fixed(new_rect.width);
        l.height = ViewSize::Fixed(new_rect.height);
        relayout_dock(
            self.root_id,
            store,
            Rect::from_lt_size(Point::new_logical(0.0, 0.0), new_rect.size()),
            context,
        );
        context.schedule_view_render(self.root_view_id);
    }

    #[tracing::instrument(skip(self, store, ctx))]
    pub fn redock(
        &self,
        source: DockID,
        store: &mut DockStore,
        index: usize,
        op: DockingOperation,
        suggested_rect: &Rect<LogicalUnit>,
        ctx: &mut RedockingContext,
    ) -> (Option<Box<dyn PaneContentPresenter>>, UndockResult) {
        store.dump(self.root_id);
        let r = redock(
            self.root_id,
            store,
            self,
            source,
            index,
            op,
            suggested_rect,
            ctx,
        );
        store.dump(self.root_id);
        r
    }

    /// Dockの構成状態をとる
    pub fn state_snapshot(&self, store: &DockStore) -> crate::DockState {
        fn rec(target: DockID, store: &DockStore) -> crate::DockState {
            match store.get(target) {
                &Dock::RootContainer { content } => rec(content, store),
                &Dock::Fill {
                    ref group_view_controller,
                    ..
                } => crate::DockState::Filled {
                    content_ids: group_view_controller
                        .contents
                        .iter()
                        .map(|c| c.presenter.id())
                        .collect(),
                    active_index: group_view_controller.current_active_index(),
                },
                &Dock::Splitted {
                    docked,
                    rest,
                    ref direction,
                    ..
                } => crate::DockState::Splitted {
                    direction: match direction {
                        DockDirection::ToLeft(width) => {
                            crate::persistence::DockDirection::Left(width.get())
                        }
                        DockDirection::ToRight(width) => {
                            crate::persistence::DockDirection::Right(width.get())
                        }
                        DockDirection::ToTop(height) => {
                            crate::persistence::DockDirection::Top(height.get())
                        }
                        DockDirection::ToBottom(height) => {
                            crate::persistence::DockDirection::Bottom(height.get())
                        }
                    },
                    content: Box::new(rec(docked, store)),
                    rest: Box::new(rec(rest, store)),
                },
            }
        }

        rec(self.root_id, store)
    }
}

/// Dockを新規に分割する
fn split_new(
    store: &mut DockStore,
    manager: &WindowDockingManager,
    view_init_ctx: &mut ViewInitContext,
    view_render_queue: &mut ViewRenderQueue,
    new_rest: DockID,
    content: Box<dyn PaneContentPresenter>,
    direction: DockDirection,
) {
    let onto = store.get(new_rest).parent().expect("no parent?");
    let new_dock = store.alloc_recurse(|parent_id, store| {
        let splitter = view_init_ctx.construct_view(
            splitter::ViewInit {
                dir: direction.splitter_direction(),
                controlling_dock: parent_id,
            },
            |_| [],
        );
        view_init_ctx.view_set_parent(splitter, manager.root_view_id);

        Dock::Splitted {
            parent: onto,
            docked: store.alloc(|id| {
                let vc = PaneGroupViewController::new(
                    &mut PaneGroupCreateContext {
                        view_init_context: view_init_ctx,
                        view_render_queue,
                    },
                    manager.root_view_id,
                    vec![content],
                    id,
                    0,
                );

                Dock::Fill {
                    parent: parent_id,
                    group_view_controller: vc,
                }
            }),
            rest: new_rest,
            splitter,
            direction,
        }
    });

    store.get_mut(new_rest).reparent(new_dock);
    store
        .get_mut(onto)
        .replace_matching_child(new_rest, new_dock);

    let relayout_base_rect = store.get_computed_state(onto).rect.clone();
    relayout_dock(
        onto,
        store,
        relayout_base_rect,
        &mut PaneContentResizeContext {
            view_instance_store: view_init_ctx.view_instance_store,
            view_render_queue,
            view_tree_relation_store: view_init_ctx.view_tree_relation_store,
        },
    );
}

/// Dockを外す
#[tracing::instrument(skip(dbg_dump_root, store, env))]
fn undock(
    dbg_dump_root: DockID,
    target: DockID,
    store: &mut DockStore,
    env: &mut (
             impl ViewRegisterable
             + ViewImmediateTeardownable
             + ViewDestructionContext
             + DeriveTeardownContext
             + DerivePaneContentResizeContext
             + ViewInstanceQueryableMut
             + ?Sized
         ),
) -> UndockResult {
    store.dump(dbg_dump_root);

    match store.free(target) {
        Dock::RootContainer { .. } => unreachable!("undocking root container"),
        Dock::Fill {
            parent,
            group_view_controller,
        } => {
            group_view_controller.destruct(env);
            let (remain_dock, parent_parent) = match store.get(parent) {
                Dock::RootContainer { .. } => {
                    // ルートにつながるDockをundockしようとしている => 何もなくなる
                    return UndockResult::ToBeEmpty;
                }
                Dock::Fill { .. } => unreachable!("fill cannot be nested"),
                &Dock::Splitted {
                    docked,
                    rest,
                    parent: parent_parent,
                    ..
                } if docked == target => (rest, parent_parent),
                &Dock::Splitted {
                    docked,
                    rest,
                    parent: parent_parent,
                    ..
                } if rest == target => (docked, parent_parent),
                t => unreachable!("invalid structure {t:?}"),
            };

            let mut remain = store.free(remain_dock);
            remain.reparent(parent_parent);
            remain.maintain_dock_id_relation(parent, env);
            match remain {
                Dock::RootContainer { content } => {
                    store.get_mut(content).reparent(parent);
                }
                Dock::Fill { .. } => {}
                Dock::Splitted { docked, rest, .. } => {
                    store.get_mut(docked).reparent(parent);
                    store.get_mut(rest).reparent(parent);
                }
            }
            store.replace(parent, remain).destruct(env);
            let relayout_base = parent_parent;
            let relayout_base_rect = store.get_computed_state(relayout_base).rect.clone();
            relayout_dock(
                relayout_base,
                store,
                relayout_base_rect,
                &mut env.derive_pane_content_resize_context(),
            );
        }
        _ => todo!(),
    }

    store.dump(dbg_dump_root);
    UndockResult::Success
}

/// Dockを移動させる
fn redock(
    dbg_dump_root: DockID,
    store: &mut DockStore,
    manager: &WindowDockingManager,
    source: DockID,
    index: usize,
    op: DockingOperation,
    suggested_rect: &Rect<LogicalUnit>,
    ctx: &mut RedockingContext,
) -> (Option<Box<dyn PaneContentPresenter>>, UndockResult) {
    let Dock::Fill {
        group_view_controller: source_group_view_controller,
        ..
    } = store.get_mut(source)
    else {
        unreachable!("merge from non-fill dock");
    };
    let content = source_group_view_controller.remove_content(index, ctx);
    let mut should_undock_source = !source_group_view_controller.has_contents();

    let diverged_contents = match op {
        // ウィンドウのオープンが必要なので内容物だけ返してLogicFiber側でやる
        DockingOperation::Diverge => Some(content),
        DockingOperation::Merge(target) => {
            if target == source {
                // 同じDockにまた帰ってくるのである状態になる
                should_undock_source = false;
            }

            let Dock::Fill {
                group_view_controller: target_group_view_controller,
                ..
            } = store.get_mut(target)
            else {
                unreachable!("merge into non-fill dock");
            };

            target_group_view_controller.add_content(
                manager.root_view_id,
                content,
                PaneGroupContentInsertion::Append,
                true,
                ctx,
            );
            let target_rect = store.get_computed_state(target).rect.clone();
            relayout_dock(
                target,
                store,
                target_rect,
                &mut PaneContentResizeContext {
                    view_instance_store: ctx.view_init_ctx.view_instance_store,
                    view_render_queue: ctx.view_render_queue,
                    view_tree_relation_store: ctx.view_init_ctx.view_tree_relation_store,
                },
            );
            None
        }
        DockingOperation::MergeAtTabIndex(target, index) => {
            if target == source {
                // 同じDockにまた帰ってくるのである状態になる
                should_undock_source = false;
            }

            let Dock::Fill {
                group_view_controller: target_group_view_controller,
                ..
            } = store.get_mut(target)
            else {
                unreachable!("merge into non-fill dock");
            };

            target_group_view_controller.add_content(
                manager.root_view_id,
                content,
                PaneGroupContentInsertion::InsertAt(index),
                true,
                ctx,
            );
            let target_rect = store.get_computed_state(target).rect.clone();
            relayout_dock(
                target,
                store,
                target_rect,
                &mut PaneContentResizeContext {
                    view_instance_store: ctx.view_init_ctx.view_instance_store,
                    view_render_queue: ctx.view_render_queue,
                    view_tree_relation_store: ctx.view_init_ctx.view_tree_relation_store,
                },
            );
            None
        }
        DockingOperation::SplitToLeft(target) => {
            split_new(
                store,
                manager,
                &mut ctx.view_init_ctx,
                ctx.view_render_queue,
                target,
                content,
                DockDirection::ToLeft(Cell::new(suggested_rect.width)),
            );
            None
        }
        DockingOperation::SplitToRight(target) => {
            split_new(
                store,
                manager,
                &mut ctx.view_init_ctx,
                ctx.view_render_queue,
                target,
                content,
                DockDirection::ToRight(Cell::new(suggested_rect.width)),
            );
            None
        }
        DockingOperation::SplitToTop(target) => {
            split_new(
                store,
                manager,
                &mut ctx.view_init_ctx,
                ctx.view_render_queue,
                target,
                content,
                DockDirection::ToTop(Cell::new(suggested_rect.height)),
            );
            None
        }
        DockingOperation::SplitToBottom(target) => {
            split_new(
                store,
                manager,
                &mut ctx.view_init_ctx,
                ctx.view_render_queue,
                target,
                content,
                DockDirection::ToBottom(Cell::new(suggested_rect.height)),
            );
            None
        }
    };

    let undock_result = if should_undock_source {
        undock(dbg_dump_root, source, store, ctx)
    } else {
        UndockResult::Success
    };

    (diverged_contents, undock_result)
}

/// Splitterの移動に関わる処理
pub fn move_splitter(
    target: DockID,
    store: &mut DockStore,
    new_splitter_client_pos: f32,
    context: &mut PaneContentResizeContext,
) {
    let self_rect = &store.get_computed_state(target).rect;
    match store.get(target) {
        Dock::RootContainer { .. } => {
            unreachable!("root container does not have any splitters!")
        }
        Dock::Fill { .. } => unreachable!("fill does not have any splitters!"),
        &Dock::Splitted {
            direction: DockDirection::ToLeft(ref width),
            splitter,
            ..
        } => {
            let root_view_offset = context
                .view_layout_untyped(
                    context
                        .view_get_parent(splitter)
                        .expect("splitter root view?"),
                )
                .expect("query failed")
                .left_offset;
            let new_splitter_pos = new_splitter_client_pos - root_view_offset;
            let new_fixed_size =
                (new_splitter_pos - self_rect.left).clamp(10.0, self_rect.width - 10.0);
            width.set(new_fixed_size);
        }
        &Dock::Splitted {
            direction: DockDirection::ToRight(ref width),
            splitter,
            ..
        } => {
            let root_view_offset = context
                .view_layout_untyped(
                    context
                        .view_get_parent(splitter)
                        .expect("splitter root view?"),
                )
                .expect("query failed")
                .left_offset;
            let new_splitter_pos = new_splitter_client_pos - root_view_offset;
            let new_fixed_size = (self_rect.right()
                - (new_splitter_pos + DESIGN_METRICS.splitter_thickness))
                .clamp(10.0, self_rect.width - 10.0);
            width.set(new_fixed_size);
        }
        &Dock::Splitted {
            direction: DockDirection::ToTop(ref height),
            splitter,
            ..
        } => {
            let root_view_offset = context
                .view_layout_untyped(
                    context
                        .view_get_parent(splitter)
                        .expect("splitter root view?"),
                )
                .expect("query failed")
                .top_offset;
            let new_splitter_pos = new_splitter_client_pos - root_view_offset;
            let new_fixed_size =
                (new_splitter_pos - self_rect.top).clamp(10.0, self_rect.height - 10.0);
            height.set(new_fixed_size);
        }
        &Dock::Splitted {
            direction: DockDirection::ToBottom(ref height),
            splitter,
            ..
        } => {
            let root_view_offset = context
                .view_layout_untyped(
                    context
                        .view_get_parent(splitter)
                        .expect("splitter root view?"),
                )
                .expect("query failed")
                .top_offset;
            let new_splitter_pos = new_splitter_client_pos - root_view_offset;
            let new_fixed_size = (self_rect.bottom()
                - (new_splitter_pos + DESIGN_METRICS.splitter_thickness))
                .clamp(10.0, self_rect.height - 10.0);
            height.set(new_fixed_size);
        }
    }

    let self_rect = self_rect.clone();
    relayout_dock(target, store, self_rect, context);
}

/// Dockのレイアウトを再帰的に再計算する
fn relayout_dock(
    target: DockID,
    store: &mut DockStore,
    available_rect: Rect<LogicalUnit>,
    context: &mut PaneContentResizeContext,
) {
    store.get_computed_state_mut(target).rect = available_rect.clone();

    match store.get(target) {
        &Dock::RootContainer { content } => relayout_dock(content, store, available_rect, context),
        &Dock::Fill {
            ref group_view_controller,
            ..
        } => group_view_controller.set_rect(available_rect, context),
        &Dock::Splitted {
            docked,
            rest,
            splitter,
            ref direction,
            ..
        } => {
            let (docked_rect, rest_rect, splitter_rect) = direction.split_rect(&available_rect);

            let l = context.view_layout_mut(splitter).expect("query failed");
            l.left_offset = splitter_rect.left;
            l.top_offset = splitter_rect.top;
            l.width = ViewSize::Fixed(splitter_rect.width);
            l.height = ViewSize::Fixed(splitter_rect.height);
            context.schedule_view_render(splitter);
            relayout_dock(docked, store, docked_rect, context);
            relayout_dock(rest, store, rest_rect, context);
        }
    }
}

pub struct DockingPreviewState {
    tab_size: Size<LogicalUnit>,
    original_rect: Rect<LogicalUnit>,
    pub offset: Point<LogicalUnit>,
    pub source_window: WindowHandle,
    pub source_dock: DockID,
    pub tab_index: usize,
}

/// RedockingのPreviewを開始する
pub fn begin_preview(
    pane_rect: Rect<LogicalUnit>,
    tab_size: Size<LogicalUnit>,
    client_pos: &Point<LogicalUnit>,
    source_window: WindowHandle,
    source_dock: DockID,
    tab_index: usize,
) -> (DockingPreviewState, Rect<LogicalUnit>) {
    let popover_rect = Rect::from_lt_size(
        Point::new_logical(pane_rect.left, pane_rect.top),
        Size::new_logical(pane_rect.width, pane_rect.height),
    );

    (
        DockingPreviewState {
            offset: Point::new_logical(pane_rect.left - client_pos.x, pane_rect.top - client_pos.y),
            tab_size,
            original_rect: pane_rect,
            source_window,
            source_dock,
            tab_index,
        },
        popover_rect,
    )
}

/// Previewを移動する
pub fn move_preview(
    root_manager: &WindowDockingManager,
    store: &DockStore,
    client_pos: &Point<LogicalUnit>,
    state: &mut DockingPreviewState,
) -> Rect<LogicalUnit> {
    compute_recommended_operation(
        root_manager.root_id,
        store,
        &state.original_rect,
        &state.tab_size,
        client_pos,
        &state.offset,
    )
    .1
}

/// Previewを終了し、確定したRedocking操作を返す
pub fn end_preview(
    root_manager: &WindowDockingManager,
    store: &DockStore,
    client_pos: &Point<LogicalUnit>,
    state: DockingPreviewState,
) -> (DockingOperation, Rect<LogicalUnit>) {
    compute_recommended_operation(
        root_manager.root_id,
        store,
        &state.original_rect,
        &state.tab_size,
        client_pos,
        &state.offset,
    )
}

const PARENT_DOCK_THRESHOLD: f32 = 8.0;
const EDGE_DOCK_THRESHOLD_RATE: f32 = 0.3;
const MAX_DOCKED_SIZE_RATE: f32 = 0.7;

/// 推奨されるRedockingの操作と、操作適用後に推奨されるPaneのジオメトリを計算する
fn compute_recommended_operation(
    this: DockID,
    store: &DockStore,
    source_rect: &Rect<LogicalUnit>,
    source_tab_size: &Size<LogicalUnit>,
    pos: &Point<LogicalUnit>,
    drag_offset: &Point<LogicalUnit>,
) -> (DockingOperation, Rect<LogicalUnit>) {
    fn try_parent_dock(
        this: DockID,
        source_rect: &Rect<LogicalUnit>,
        dock_rect: &Rect<LogicalUnit>,
        pos: &Point<LogicalUnit>,
    ) -> Option<(DockingOperation, Rect<LogicalUnit>)> {
        if pos.x <= dock_rect.left + PARENT_DOCK_THRESHOLD {
            return Some((
                DockingOperation::SplitToLeft(this),
                dock_rect.slice_left(source_rect.width.min(dock_rect.width * 0.7)),
            ));
        }
        if pos.x >= dock_rect.right() - PARENT_DOCK_THRESHOLD {
            return Some((
                DockingOperation::SplitToRight(this),
                dock_rect.slice_right(source_rect.width.min(dock_rect.width * 0.7)),
            ));
        }
        if pos.y <= dock_rect.top + PARENT_DOCK_THRESHOLD {
            return Some((
                DockingOperation::SplitToTop(this),
                dock_rect.slice_top(source_rect.height.min(dock_rect.height * 0.7)),
            ));
        }
        if pos.y >= dock_rect.bottom() - PARENT_DOCK_THRESHOLD {
            return Some((
                DockingOperation::SplitToBottom(this),
                dock_rect.slice_bottom(source_rect.height.min(dock_rect.height * 0.7)),
            ));
        }

        None
    }

    let dock_rect = &store.get_computed_state(this).rect;
    if !dock_rect.point_in_inclusive(&pos) {
        // not hit to the rect
        return (
            DockingOperation::Diverge,
            source_rect.ref_relocate(&pos.with_offset(drag_offset.clone())),
        );
    }

    match store.get(this) {
        &Dock::RootContainer { content } => {
            return compute_recommended_operation(
                content,
                store,
                source_rect,
                source_tab_size,
                pos,
                drag_offset,
            );
        }
        Dock::Fill {
            group_view_controller,
            ..
        } => {
            if pos.y <= dock_rect.top + DESIGN_METRICS.tab_height() {
                // dock to tab index
                let local_pos = Point::new_logical(pos.x - dock_rect.left, pos.y - dock_rect.top);
                let (index, tab_lt) = group_view_controller.hittest_tab_index(local_pos);

                return (
                    DockingOperation::MergeAtTabIndex(this, index),
                    Rect::from_lt_size(
                        tab_lt.with_offset(dock_rect.left_top()),
                        source_tab_size.clone(),
                    ),
                );
            }

            let dl = pos.x - dock_rect.left;
            let dr = dock_rect.right() - pos.x;
            let dt = pos.y - dock_rect.top;
            let db = dock_rect.bottom() - pos.y;
            if dl.min(dr) < dt.min(db) {
                if dl <= dock_rect.width * EDGE_DOCK_THRESHOLD_RATE {
                    return (
                        DockingOperation::SplitToLeft(this),
                        dock_rect.slice_left(
                            source_rect
                                .width
                                .min(dock_rect.width * MAX_DOCKED_SIZE_RATE),
                        ),
                    );
                }
                if dr <= dock_rect.width * EDGE_DOCK_THRESHOLD_RATE {
                    return (
                        DockingOperation::SplitToRight(this),
                        dock_rect.slice_right(
                            source_rect
                                .width
                                .min(dock_rect.width * MAX_DOCKED_SIZE_RATE),
                        ),
                    );
                }
            } else {
                if dt <= dock_rect.height * EDGE_DOCK_THRESHOLD_RATE {
                    return (
                        DockingOperation::SplitToTop(this),
                        dock_rect.slice_top(
                            source_rect
                                .height
                                .min(dock_rect.height * MAX_DOCKED_SIZE_RATE),
                        ),
                    );
                }
                if db <= dock_rect.height * EDGE_DOCK_THRESHOLD_RATE {
                    return (
                        DockingOperation::SplitToBottom(this),
                        dock_rect.slice_bottom(
                            source_rect
                                .height
                                .min(dock_rect.height * MAX_DOCKED_SIZE_RATE),
                        ),
                    );
                }
            }

            return (DockingOperation::Merge(this), dock_rect.clone());
        }
        &Dock::Splitted {
            docked,
            rest,
            direction: DockDirection::ToLeft(ref width),
            ..
        } => {
            if let Some(op) = try_parent_dock(this, &source_rect, &dock_rect, &pos) {
                // dock to parent
                return op;
            }

            let width = width.get();
            let r = dock_rect.slice_left(width);
            if pos.x <= r.right() {
                return compute_recommended_operation(
                    docked,
                    store,
                    source_rect,
                    source_tab_size,
                    pos,
                    drag_offset,
                );
            }
            let r =
                dock_rect.slice_right(dock_rect.width - width - DESIGN_METRICS.splitter_thickness);
            if pos.x >= r.left {
                return compute_recommended_operation(
                    rest,
                    store,
                    source_rect,
                    source_tab_size,
                    pos,
                    drag_offset,
                );
            }
        }
        &Dock::Splitted {
            docked,
            rest,
            direction: DockDirection::ToRight(ref width),
            ..
        } => {
            if let Some(op) = try_parent_dock(this, &source_rect, &dock_rect, &pos) {
                // dock to parent
                return op;
            }

            let width = width.get();
            let r =
                dock_rect.slice_left(dock_rect.width - width - DESIGN_METRICS.splitter_thickness);
            if pos.x <= r.right() {
                return compute_recommended_operation(
                    rest,
                    store,
                    source_rect,
                    source_tab_size,
                    pos,
                    drag_offset,
                );
            }
            let r = dock_rect.slice_right(width);
            if pos.x >= r.left {
                return compute_recommended_operation(
                    docked,
                    store,
                    source_rect,
                    source_tab_size,
                    pos,
                    drag_offset,
                );
            }
        }
        &Dock::Splitted {
            docked,
            rest,
            direction: DockDirection::ToTop(ref height),
            ..
        } => {
            if let Some(op) = try_parent_dock(this, &source_rect, &dock_rect, &pos) {
                // dock to parent
                return op;
            }

            let height = height.get();
            let r = dock_rect.slice_top(height);
            if pos.y <= r.bottom() {
                return compute_recommended_operation(
                    docked,
                    store,
                    source_rect,
                    source_tab_size,
                    pos,
                    drag_offset,
                );
            }
            let r = dock_rect
                .slice_bottom(dock_rect.height - height - DESIGN_METRICS.splitter_thickness);
            if pos.y >= r.top {
                return compute_recommended_operation(
                    rest,
                    store,
                    source_rect,
                    source_tab_size,
                    pos,
                    drag_offset,
                );
            }
        }
        &Dock::Splitted {
            docked,
            rest,
            direction: DockDirection::ToBottom(ref height),
            ..
        } => {
            if let Some(op) = try_parent_dock(this, &source_rect, &dock_rect, &pos) {
                // dock to parent
                return op;
            }

            let height = height.get();
            let r =
                dock_rect.slice_top(dock_rect.height - height - DESIGN_METRICS.splitter_thickness);
            if pos.y <= r.bottom() {
                return compute_recommended_operation(
                    rest,
                    store,
                    source_rect,
                    source_tab_size,
                    pos,
                    drag_offset,
                );
            }
            let r = dock_rect.slice_bottom(height);
            if pos.y >= r.top {
                return compute_recommended_operation(
                    docked,
                    store,
                    source_rect,
                    source_tab_size,
                    pos,
                    drag_offset,
                );
            }
        }
    }

    (
        DockingOperation::Diverge,
        source_rect.ref_relocate(&pos.with_offset(drag_offset.clone())),
    )
}

pub struct PaneGroupCreateContext<'env, 'a, 'sys> {
    pub view_init_context: &'env mut ViewInitContext<'a, 'sys>,
    pub view_render_queue: &'env mut ViewRenderQueue,
}
impl ViewRegisterable for PaneGroupCreateContext<'_, '_, '_> {
    #[inline(always)]
    fn construct_view_direct<T: View + 'static>(
        &mut self,
        ctor: impl FnOnce(TypedViewIdentifier<T>) -> Box<T>,
    ) -> TypedViewIdentifier<T> {
        self.view_init_context.construct_view_direct(ctor)
    }

    #[inline(always)]
    fn free_view_untyped(&mut self, id: ViewIdentifier) {
        self.view_init_context.free_view_untyped(id)
    }
}
impl ViewRelationControllable for PaneGroupCreateContext<'_, '_, '_> {
    #[inline(always)]
    fn view_set_parent_untyped(&mut self, id: ViewIdentifier, parent: ViewIdentifier) {
        crate::uicore::view_set_parent(id, parent, self.view_init_context.view_tree_relation_store);
    }

    #[inline(always)]
    fn view_detach_parent_untyped(&mut self, id: ViewIdentifier) {
        crate::uicore::view_detach_parent(id, self.view_init_context.view_tree_relation_store);
    }
}
impl ViewRenderer for PaneGroupCreateContext<'_, '_, '_> {
    #[inline(always)]
    fn schedule_view_render_untyped(&mut self, target: ViewIdentifier) {
        self.view_render_queue.schedule(target);
    }
}
impl ViewInstanceQueryable for PaneGroupCreateContext<'_, '_, '_> {
    #[inline(always)]
    fn view_instance_of<T: View + 'static>(&self, id: ViewIdentifier) -> Option<&T> {
        self.view_init_context.view_instance_of(id)
    }

    #[inline(always)]
    fn view_layout_untyped(&self, id: ViewIdentifier) -> Option<&ViewLayout> {
        crate::uicore::view_layout(id, self.view_init_context.view_instance_store)
    }
}
impl ViewInstanceQueryableMut for PaneGroupCreateContext<'_, '_, '_> {
    #[inline(always)]
    fn view_instance_mut_of<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T> {
        self.view_init_context.view_instance_mut_of(id)
    }

    #[inline(always)]
    fn view_set_visibility_untyped(&mut self, id: ViewIdentifier, visible: bool) {
        crate::uicore::view_set_visibility(id, visible, self.view_init_context.view_instance_store);
    }

    #[inline(always)]
    fn view_layout_mut_untyped(&mut self, id: ViewIdentifier) -> Option<&mut ViewLayout> {
        crate::uicore::view_layout_mut(id, self.view_init_context.view_instance_store)
    }
}
impl<'sys> SystemLinkAccess<'sys> for PaneGroupCreateContext<'_, '_, 'sys> {
    #[inline(always)]
    fn system_link<'a>(&'a self) -> &'a SystemLink<'sys> {
        self.view_init_context.system_link
    }
}

/// PaneGroupContainerViewの初期データ
struct PaneGroupContainerViewInit;
impl ViewConstructor for PaneGroupContainerViewInit {
    type ConcreteView = PaneGroupContainerView;

    fn construct(self, _id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        PaneGroupContainerView { entity: None }
    }
}

/// Paneの内容が乗るContainerとしてのView
struct PaneGroupContainerView {
    entity: Option<PaneGroupContainerViewEntity>,
}
impl Drop for PaneGroupContainerView {
    fn drop(&mut self) {
        if self.entity.is_some() {
            tracing::warn!("PaneGroupContainerView dropped without teardown");
        }
    }
}
impl View for PaneGroupContainerView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => {
                // placement changed
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
                    .composite_fill_color_imm([1.0, 1.0, 1.0, 0.0625])
                    .clip_child_hard()
                    .create(ctx.composite_tree);
                let ht_root = HitTestTreeData::build()
                    .rect(layout_rect)
                    .create(ctx.ht_manager);

                &*self
                    .entity
                    .insert(PaneGroupContainerViewEntity { ct_root, ht_root })
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

        ctx.composite_tree.free(entity.ct_root);
        ctx.ht_manager.free(entity.ht_root);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }

    fn create_new_layout_layer(&self) -> bool {
        true
    }
}

struct PaneGroupContainerViewEntity {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}

/// PaneGroupにContentをInsertする方法
enum PaneGroupContentInsertion {
    /// 追加
    Append,
    /// 指定位置に挿入
    InsertAt(usize),
}

/// Paneのグループ
pub struct PaneGroupViewController {
    /// このグループが乗っているDockのID
    dock: DockID,
    /// タブ部分
    tab_strip_view: TypedViewIdentifier<PaneGroupTabStripView>,
    /// このグループに所属しているタブとPresenterのインスタンスのリスト
    contents: Vec<PaneGroupContent>,
    /// 現在アクティブなタブのViewID
    current_active_tab_view: TypedViewIdentifier<PaneGroupTabView>,
}
impl PaneGroupViewController {
    /// 生成
    pub fn new(
        ctx: &mut PaneGroupCreateContext,
        parent_view: TypedViewIdentifier<WindowDockRootView>,
        contents: Vec<Box<dyn PaneContentPresenter>>,
        dock: DockID,
        initial_active_index: usize,
    ) -> Self {
        let tab_strip_view = ctx.construct_view(PaneGroupTabStripViewInit, |_| []);
        ctx.view_set_parent(tab_strip_view, parent_view);

        let initial_active_index = initial_active_index.clamp(0, contents.len() - 1);
        let contents = contents
            .into_iter()
            .enumerate()
            .map(|(index, c)| {
                let is_active_tab = index == initial_active_index;
                let tab_name = c.name();
                let tab_width = PaneGroupTabView::compute_width(&tab_name, ctx.system_link());
                let tab_view = ctx.construct_view(
                    PaneGroupTabViewInit {
                        label: tab_name,
                        dock,
                        active: is_active_tab,
                    },
                    |_| [],
                );
                let container =
                    ctx.construct_view(PaneGroupContainerViewInit, |_| [c.root_view_id()]);

                ctx.view_set_visibility(container, is_active_tab);
                ctx.view_set_parent(tab_view, tab_strip_view);
                ctx.view_set_parent(container, parent_view);

                PaneGroupContent {
                    container,
                    presenter: c,
                    tab_view,
                    tab_width,
                }
            })
            .collect::<Vec<_>>();

        Self::relocate_tabs(contents.iter().map(|x| (x.tab_view, x.tab_width)), ctx);

        Self {
            current_active_tab_view: contents[initial_active_index].tab_view,
            dock,
            tab_strip_view,
            contents,
        }
    }

    /// 後始末
    fn destruct(
        mut self,
        env: &mut (impl ViewRegisterable + ViewDestructionContext + DeriveTeardownContext + ?Sized),
    ) {
        for mut x in self.contents.drain(..) {
            x.presenter.teardown(&mut env.derive_teardown_context());

            env.destruct_view_recursive(x.tab_view);
            env.destruct_view_recursive(x.container);
        }

        env.destruct_view_recursive(self.tab_strip_view);
    }

    /// 矩形を設定する
    pub fn set_rect(&self, rect: Rect<LogicalUnit>, context: &mut PaneContentResizeContext) {
        let tab_strip_rect = rect.slice_top(DESIGN_METRICS.tab_height());
        let content_rect = rect.slice_bottom(rect.height - DESIGN_METRICS.tab_height());
        let content_size = content_rect.size();

        let l = context
            .view_layout_mut(self.tab_strip_view)
            .expect("query failed");
        l.left_offset = tab_strip_rect.left;
        l.top_offset = tab_strip_rect.top;
        l.width = ViewSize::Fixed(tab_strip_rect.width);
        context.schedule_view_render(self.tab_strip_view);

        for x in self.contents.iter() {
            let l = context.view_layout_mut(x.container).expect("query failed");
            l.left_offset = content_rect.left;
            l.top_offset = content_rect.top;
            l.width = ViewSize::Fixed(content_rect.width);
            l.height = ViewSize::Fixed(content_rect.height);
            context.schedule_view_render(x.container);
            x.presenter.resize(&content_size, context);
        }
    }

    /// このグループが乗っているDockを変更する
    fn rebind_dock(&mut self, dock: DockID, env: &mut (impl ViewInstanceQueryableMut + ?Sized)) {
        self.dock = dock;
        for x in self.contents.iter() {
            env.view_instance_mut::<PaneGroupTabView>(x.tab_view)
                .expect("query failed")
                .rebind_dock(dock);
        }
    }

    /// タブとのヒットテストを行い、ヒットしたタブのインデックス番号と左上の座標を返す
    fn hittest_tab_index(&self, pos: Point<LogicalUnit>) -> (usize, Point<LogicalUnit>) {
        if pos.x < 0.0 {
            return (0, Point::new_logical(0.0, 0.0));
        }

        let mut leftmost = 0.0;
        for (index, x) in self.contents.iter().enumerate() {
            if leftmost <= pos.x && pos.x <= leftmost + x.tab_width {
                return (index, Point::new_logical(leftmost, 0.0));
            }
            leftmost += x.tab_width;
        }

        (self.contents.len(), Point::new_logical(leftmost, 0.0))
    }

    /// コンテンツを追加する
    fn add_content<'a, 'sys>(
        &mut self,
        dock_root_view: TypedViewIdentifier<WindowDockRootView>,
        content: Box<dyn PaneContentPresenter>,
        method: PaneGroupContentInsertion,
        with_activate: bool,
        env: &mut (
                 impl ViewRegisterable
                 + ViewRenderer
                 + ViewInstanceQueryableMut
                 + SystemLinkAccess<'sys>
                 + ViewRelationControllable
                 + ?Sized
             ),
    ) {
        let tab_name = content.name();
        let tab_width = PaneGroupTabView::compute_width(&tab_name, env.system_link());
        let tab_view = env.construct_view(
            PaneGroupTabViewInit {
                label: tab_name,
                dock: self.dock,
                active: with_activate,
            },
            |_| [],
        );
        let container =
            env.construct_view(PaneGroupContainerViewInit, |_| [content.root_view_id()]);
        env.view_set_parent(tab_view, self.tab_strip_view);
        env.view_set_parent(container, dock_root_view);

        let activation_index = match method {
            PaneGroupContentInsertion::Append => {
                self.contents.push(PaneGroupContent {
                    container,
                    presenter: content,
                    tab_view,
                    tab_width,
                });
                self.contents.len() - 1
            }
            PaneGroupContentInsertion::InsertAt(index) => {
                self.contents.insert(
                    index,
                    PaneGroupContent {
                        container,
                        presenter: content,
                        tab_view,
                        tab_width,
                    },
                );
                index
            }
        };

        Self::relocate_tabs(self.contents.iter().map(|x| (x.tab_view, x.tab_width)), env);

        if with_activate {
            self.select_tab(self.contents[activation_index].tab_view, env);
        }
    }

    /// コンテンツを削除する
    fn remove_content<'a>(
        &mut self,
        index: usize,
        env: &mut (
                 impl ViewRenderer
                 + ViewInstanceQueryableMut
                 + ViewRegisterable
                 + ViewDestructionContext
                 + ViewRelationControllable
                 + ?Sized
             ),
    ) -> Box<dyn PaneContentPresenter> {
        let content_set = self.contents.remove(index);

        // presenterだけ生かして他はdestruct
        env.view_detach_parent_untyped(content_set.presenter.root_view_id());
        env.destruct_view_recursive(content_set.container);
        env.destruct_view_recursive(content_set.tab_view);

        if self.current_active_tab_view == content_set.tab_view && !self.contents.is_empty() {
            // activate another content
            let new_active = index.clamp(0, self.contents.len() - 1);
            env.view_instance_mut(self.contents[new_active].tab_view)
                .expect("query failed")
                .set_active(true);
            env.schedule_view_render(self.contents[new_active].tab_view);
            env.view_set_visibility(self.contents[new_active].container, true);
            env.schedule_view_render(self.contents[new_active].container);

            self.current_active_tab_view = self.contents[new_active].tab_view;
        }

        Self::relocate_tabs(self.contents.iter().map(|x| (x.tab_view, x.tab_width)), env);
        content_set.presenter
    }

    /// コンテンツが一つ以上存在するかどうかを返す
    #[inline(always)]
    fn has_contents(&self) -> bool {
        !self.contents.is_empty()
    }

    /// タブを再配置する
    fn relocate_tabs<'t>(
        tab_views_with_width: impl Iterator<Item = (TypedViewIdentifier<PaneGroupTabView>, f32)>,
        env: &mut (impl ViewInstanceQueryableMut + ViewRenderer + ?Sized),
    ) {
        let mut left_offset = 0.0;
        for (t, w) in tab_views_with_width {
            env.view_instance_mut(t)
                .expect("query failed")
                .place(Point::new_logical(left_offset, 0.0));
            env.schedule_view_render(t);
            left_offset += w;
        }
    }

    /// タブViewのIDからインデックスを計算する
    #[inline(always)]
    fn tab_index(&self, tab: TypedViewIdentifier<PaneGroupTabView>) -> Option<usize> {
        self.contents.iter().position(|x| x.tab_view == tab)
    }

    #[inline(always)]
    fn current_active_index(&self) -> usize {
        self.tab_index(self.current_active_tab_view)
            .expect("invalid tab active")
    }

    /// タブを選択する
    fn select_tab(
        &mut self,
        tab: TypedViewIdentifier<PaneGroupTabView>,
        env: &mut (impl ViewInstanceQueryableMut + ViewRenderer + ?Sized),
    ) {
        let old_active_tab_view = core::mem::replace(&mut self.current_active_tab_view, tab);
        if old_active_tab_view == self.current_active_tab_view {
            // tab not changed
            return;
        }

        let old_index = self
            .contents
            .iter()
            .position(|x| x.tab_view == old_active_tab_view)
            .expect("invalid tab selected");
        let new_index = self
            .contents
            .iter()
            .position(|x| x.tab_view == self.current_active_tab_view)
            .expect("invalid tab selected");

        env.view_instance_mut(old_active_tab_view)
            .expect("query failed")
            .set_active(false);
        env.schedule_view_render(old_active_tab_view);
        env.view_instance_mut(self.current_active_tab_view)
            .expect("query failed")
            .set_active(true);
        env.schedule_view_render(self.current_active_tab_view);

        env.view_set_visibility(self.contents[old_index].container, false);
        env.schedule_view_render(self.contents[old_index].container);
        env.view_set_visibility(self.contents[new_index].container, true);
        env.schedule_view_render(self.contents[new_index].container);
    }
}

/// PaneGroupのコンテンツごとの情報
struct PaneGroupContent {
    /// Containre View
    container: TypedViewIdentifier<PaneGroupContainerView>,
    /// Presenter
    presenter: Box<dyn PaneContentPresenter>,
    /// タブViewのID
    tab_view: TypedViewIdentifier<PaneGroupTabView>,
    /// タブViewの幅
    tab_width: f32,
}
