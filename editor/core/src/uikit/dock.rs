use core::cell::{Cell, RefCell};
use std::{collections::BTreeSet, rc::Rc};

use crate::{
    Event, LogicFiberEventDispatcher, SyncEvent, WindowHandle,
    input::{
        EventContinueControl, InputEventContext, PointerInputUnit,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager,
            HitTestTreeRef, PointerActionArgs, PointerButton, PointerButtonActionArgs,
        },
    },
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, ClipConfig, CompositeMode,
            CompositeRect, CompositeRectScaleFactor, CompositeRectText,
            CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTree, CompositeTreeRef, CornerRadius,
            Gradient, GradientRef,
        },
        text::{FontID, TextLayout},
    },
    uikit::{
        MountContext, MountTarget, RawMountTarget, TeardownContext, ViewEventHandler,
        ViewIdentifier, ViewInitContext, ViewUpdateContext,
    },
    utils::{LogicalUnit, Point, Rect, SafeF32, Size, UnsafeMainThreadOnlyOnceCell},
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

/// Paneの表示内容
pub trait PaneContentPresenter {
    /// タブ名
    fn name(&self) -> String;
    /// マウント
    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget);
    /// アンマウント
    fn unmount(&self, ctx: &mut MountContext);
    /// 後始末
    fn teardown(&mut self, ctx: &mut TeardownContext);

    /// サイズ変更
    #[allow(unused_variables)]
    #[inline(always)]
    fn resize(
        &self,
        new_size: &Size<LogicalUnit>,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
    }
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

    // pub fn replace_by(&mut self, id: DockID, f: impl FnOnce(Dock) -> Dock) {
    //     let x = unsafe {
    //         core::ptr::read(
    //             self.docks[id.store_index()]
    //                 .as_ref()
    //                 .expect("already freed?"),
    //         )
    //     };
    //     core::mem::forget(core::mem::replace(
    //         self.docks[id.store_index()]
    //             .as_mut()
    //             .expect("already freed?"),
    //         f(x),
    //     ));
    // }

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
        parent: DockID,
        view_init_ctx: &mut ViewInitContext,
        contents: impl FnOnce(&mut ViewInitContext) -> Vec<Box<dyn PaneContentPresenter>>,
    ) -> DockID {
        self.alloc(move |id| {
            let contents = contents(view_init_ctx);

            Dock::Fill {
                group_view: PaneGroupView::new(view_init_ctx, contents, id),
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
    const fn splitter_direction(&self) -> DockedPaneSplitDirection {
        match self {
            Self::ToLeft(_) | Self::ToRight(_) => DockedPaneSplitDirection::Horizontal,
            Self::ToTop(_) | Self::ToBottom(_) => DockedPaneSplitDirection::Vertical,
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
        group_view: PaneGroupView,
    },
    Splitted {
        parent: DockID,
        docked: DockID,
        rest: DockID,
        splitter: DockedPaneSplitterView,
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
    fn teardown(self, ctx: &mut TeardownContext) {
        match self {
            Self::RootContainer { .. } => {}
            Self::Fill { group_view, .. } => group_view.teardown(ctx),
            Self::Splitted { splitter, .. } => splitter.teardown(ctx),
        }
    }

    fn mount(&self, ctx: &mut MountContext, mount_target: &(impl MountTarget + ?Sized)) {
        match self {
            Self::RootContainer { .. } => {}
            Self::Fill { group_view, .. } => group_view.mount(ctx, mount_target),
            Self::Splitted { splitter, .. } => splitter.mount(ctx, mount_target),
        }
    }

    const fn parent(&self) -> Option<DockID> {
        match self {
            &Self::RootContainer { .. } => None,
            &Self::Fill { parent, .. } => Some(parent),
            &Self::Splitted { parent, .. } => Some(parent),
        }
    }

    fn maintain_dock_id_relation(&self, id: DockID) {
        match self {
            Self::RootContainer { .. } => {}
            Self::Fill { group_view, .. } => group_view.rebind_dock(id),
            Self::Splitted { splitter, .. } => splitter.rebind_controlling_dock(id),
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

pub struct RedockingContext<'a, 'h> {
    pub view_init_ctx: ViewInitContext<'a, 'h>,
}
impl<'a, 'h> RedockingContext<'a, 'h> {
    fn make_teardown_context(&mut self) -> TeardownContext<'_, 'h> {
        TeardownContext {
            mount_context: MountContext {
                composite_tree: self.view_init_ctx.mount_context.composite_tree,
                ht_manager: self.view_init_ctx.mount_context.ht_manager,
                keyboard_focus_registry: self.view_init_ctx.mount_context.keyboard_focus_registry,
                current_sec: self.view_init_ctx.mount_context.current_sec,
            },
        }
    }
}

pub struct DockingManager {
    root_id: DockID,
}
impl DockingManager {
    #[tracing::instrument(skip(bound_window, ctx, store, dock_ctor))]
    pub fn new(
        bound_window: WindowHandle,
        ctx: &mut ViewInitContext,
        max_rect: Rect<LogicalUnit>,
        store: &mut DockStore,
        dock_ctor: impl FnOnce(&mut ViewInitContext, &mut DockStore) -> DockID,
    ) -> Self {
        let root_id = dock_ctor(ctx, store);
        mount_recursive(root_id, store, ctx, &bound_window);
        relayout_dock(
            root_id,
            store,
            max_rect,
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );

        Self { root_id }
    }

    #[inline(always)]
    pub fn resize(
        &self,
        new_rect: Rect<LogicalUnit>,
        store: &mut DockStore,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        relayout_dock(self.root_id, store, new_rect, composite_tree, ht_manager);
    }

    #[tracing::instrument(skip(self, store, ctx, mount_target))]
    pub fn redock(
        &self,
        source: DockID,
        store: &mut DockStore,
        index: usize,
        op: DockingOperation,
        suggested_rect: &Rect<LogicalUnit>,
        ctx: &mut RedockingContext,
        mount_target: &(impl MountTarget + ?Sized),
    ) -> (Option<Box<dyn PaneContentPresenter>>, UndockResult) {
        store.dump(self.root_id);
        let r = redock(
            self.root_id,
            store,
            source,
            index,
            op,
            suggested_rect,
            ctx,
            mount_target,
        );
        store.dump(self.root_id);
        r
    }
}

/// Dockの内容を再帰的にmountする
fn mount_recursive(
    target: DockID,
    store: &DockStore,
    ctx: &mut MountContext,
    mount_target: &(impl MountTarget + ?Sized),
) {
    match store.get(target) {
        &Dock::RootContainer { content } => {
            mount_recursive(content, store, ctx, mount_target);
        }
        &Dock::Fill { ref group_view, .. } => group_view.mount(ctx, mount_target),
        &Dock::Splitted {
            docked,
            rest,
            ref splitter,
            ..
        } => {
            mount_recursive(docked, store, ctx, mount_target);
            mount_recursive(rest, store, ctx, mount_target);
            splitter.mount(ctx, mount_target);
        }
    }
}

/// Dockを新規に分割する
fn split_new(
    store: &mut DockStore,
    view_init_ctx: &mut ViewInitContext,
    mount_target: &(impl MountTarget + ?Sized),
    new_rest: DockID,
    content: Box<dyn PaneContentPresenter>,
    direction: DockDirection,
) {
    let onto = store.get(new_rest).parent().expect("no parent?");
    let new_dock = store.alloc_recurse(|parent_id, store| {
        let d = Dock::Splitted {
            parent: onto,
            docked: store.alloc(|id| {
                let d = Dock::Fill {
                    parent: parent_id,
                    group_view: PaneGroupView::new(view_init_ctx, vec![content], id),
                };
                d.mount(view_init_ctx, mount_target);
                d
            }),
            rest: new_rest,
            splitter: DockedPaneSplitterView::new(
                view_init_ctx,
                direction.splitter_direction(),
                parent_id,
            ),
            direction,
        };
        d.mount(view_init_ctx, mount_target);
        d
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
        view_init_ctx.mount_context.composite_tree,
        view_init_ctx.mount_context.ht_manager,
    );
}

/// Dockを外す
#[tracing::instrument(skip(dbg_dump_root, store, teardown_ctx))]
fn undock(
    dbg_dump_root: DockID,
    target: DockID,
    store: &mut DockStore,
    teardown_ctx: &mut TeardownContext,
) -> UndockResult {
    store.dump(dbg_dump_root);

    match store.free(target) {
        Dock::RootContainer { .. } => unreachable!("undocking root container"),
        Dock::Fill { parent, group_view } => {
            group_view.teardown(teardown_ctx);
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
            remain.maintain_dock_id_relation(parent);
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
            store.replace(parent, remain).teardown(teardown_ctx);
            let relayout_base = parent_parent;
            let relayout_base_rect = store.get_computed_state(relayout_base).rect.clone();
            relayout_dock(
                relayout_base,
                store,
                relayout_base_rect,
                teardown_ctx.mount_context.composite_tree,
                teardown_ctx.mount_context.ht_manager,
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
    source: DockID,
    index: usize,
    op: DockingOperation,
    suggested_rect: &Rect<LogicalUnit>,
    ctx: &mut RedockingContext,
    mount_target: &(impl MountTarget + ?Sized),
) -> (Option<Box<dyn PaneContentPresenter>>, UndockResult) {
    let Dock::Fill {
        group_view: source_group_view,
        ..
    } = store.get(source)
    else {
        unreachable!("merge from non-fill dock");
    };
    let content = source_group_view.remove_content(index, &mut ctx.make_teardown_context());
    let mut should_undock_source = !source_group_view.has_contents();

    let diverged_contents = match op {
        // ウィンドウのオープンが必要なので内容物だけ返してLogicFiber側でやる
        DockingOperation::Diverge => Some(content),
        DockingOperation::Merge(target) => {
            if target == source {
                // 同じDockにまた帰ってくるのである状態になる
                should_undock_source = false;
            }

            let Dock::Fill {
                group_view: target_group_view,
                ..
            } = store.get(target)
            else {
                unreachable!("merge into non-fill dock");
            };

            target_group_view.add_content(content, &mut ctx.view_init_ctx, true);
            let target_rect = store.get_computed_state(target).rect.clone();
            relayout_dock(
                target,
                store,
                target_rect,
                ctx.view_init_ctx.mount_context.composite_tree,
                ctx.view_init_ctx.mount_context.ht_manager,
            );
            None
        }
        DockingOperation::MergeAtTabIndex(target, index) => {
            if target == source {
                // 同じDockにまた帰ってくるのである状態になる
                should_undock_source = false;
            }

            let Dock::Fill {
                group_view: target_group_view,
                ..
            } = store.get(target)
            else {
                unreachable!("merge into non-fill dock");
            };

            target_group_view.insert_content(content, index, &mut ctx.view_init_ctx, true);
            let target_rect = store.get_computed_state(target).rect.clone();
            relayout_dock(
                target,
                store,
                target_rect,
                ctx.view_init_ctx.mount_context.composite_tree,
                ctx.view_init_ctx.mount_context.ht_manager,
            );
            None
        }
        DockingOperation::SplitToLeft(target) => {
            split_new(
                store,
                &mut ctx.view_init_ctx,
                mount_target,
                target,
                content,
                DockDirection::ToLeft(Cell::new(suggested_rect.width)),
            );
            None
        }
        DockingOperation::SplitToRight(target) => {
            split_new(
                store,
                &mut ctx.view_init_ctx,
                mount_target,
                target,
                content,
                DockDirection::ToRight(Cell::new(suggested_rect.width)),
            );
            None
        }
        DockingOperation::SplitToTop(target) => {
            split_new(
                store,
                &mut ctx.view_init_ctx,
                mount_target,
                target,
                content,
                DockDirection::ToTop(Cell::new(suggested_rect.height)),
            );
            None
        }
        DockingOperation::SplitToBottom(target) => {
            split_new(
                store,
                &mut ctx.view_init_ctx,
                mount_target,
                target,
                content,
                DockDirection::ToBottom(Cell::new(suggested_rect.height)),
            );
            None
        }
    };

    let undock_result = if should_undock_source {
        undock(
            dbg_dump_root,
            source,
            store,
            &mut ctx.make_teardown_context(),
        )
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
    composite_tree: &mut CompositeTree<SyncEvent>,
    ht_manager: &mut HitTestTreeManager,
) {
    let self_rect = &store.get_computed_state(target).rect;
    match store.get(target) {
        Dock::RootContainer { .. } => {
            unreachable!("root container does not have any splitters!")
        }
        Dock::Fill { .. } => unreachable!("fill does not have any splitters!"),
        Dock::Splitted {
            direction: DockDirection::ToLeft(width),
            ..
        } => {
            let new_fixed_size =
                (new_splitter_client_pos - self_rect.left).clamp(10.0, self_rect.width - 10.0);
            width.set(new_fixed_size);
        }
        Dock::Splitted {
            direction: DockDirection::ToRight(width),
            ..
        } => {
            let new_fixed_size = (self_rect.right()
                - (new_splitter_client_pos + DESIGN_METRICS.splitter_thickness))
                .clamp(10.0, self_rect.width - 10.0);
            width.set(new_fixed_size);
        }
        Dock::Splitted {
            direction: DockDirection::ToTop(height),
            ..
        } => {
            let new_fixed_size =
                (new_splitter_client_pos - self_rect.top).clamp(10.0, self_rect.height - 10.0);
            height.set(new_fixed_size);
        }
        Dock::Splitted {
            direction: DockDirection::ToBottom(height),
            ..
        } => {
            let new_fixed_size = (self_rect.bottom()
                - (new_splitter_client_pos + DESIGN_METRICS.splitter_thickness))
                .clamp(10.0, self_rect.height - 10.0);
            height.set(new_fixed_size);
        }
    }

    let self_rect = self_rect.clone();
    relayout_dock(target, store, self_rect, composite_tree, ht_manager);
}

/// Dockのレイアウトを再帰的に再計算する
fn relayout_dock(
    target: DockID,
    store: &mut DockStore,
    available_rect: Rect<LogicalUnit>,
    composite_tree: &mut CompositeTree<SyncEvent>,
    ht_manager: &mut HitTestTreeManager,
) {
    store.get_computed_state_mut(target).rect = available_rect.clone();

    match store.get(target) {
        &Dock::RootContainer { content } => {
            relayout_dock(content, store, available_rect, composite_tree, ht_manager)
        }
        &Dock::Fill { ref group_view, .. } => {
            group_view.set_rect(available_rect, composite_tree, ht_manager)
        }
        &Dock::Splitted {
            docked,
            rest,
            ref splitter,
            ref direction,
            ..
        } => {
            let (docked_rect, rest_rect, splitter_rect) = direction.split_rect(&available_rect);

            splitter.resize(splitter_rect, composite_tree, ht_manager);
            relayout_dock(docked, store, docked_rect, composite_tree, ht_manager);
            relayout_dock(rest, store, rest_rect, composite_tree, ht_manager);
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
    root_manager: &DockingManager,
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
    root_manager: &DockingManager,
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
        Dock::Fill { group_view, .. } => {
            if pos.y <= dock_rect.top + DESIGN_METRICS.tab_height() {
                // dock to tab index
                let local_pos = Point::new_logical(pos.x - dock_rect.left, pos.y - dock_rect.top);
                let (index, tab_lt) = group_view.hittest_tab_index(local_pos);

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

/// Pane分割方向
#[derive(Clone, Copy)]
pub enum DockedPaneSplitDirection {
    /// 横
    Horizontal,
    /// 縦
    Vertical,
}
impl DockedPaneSplitDirection {
    /// Splitterに適切なカーソル形状を得る
    const fn cursor_shape(&self) -> CursorShape {
        match self {
            Self::Horizontal => CursorShape::ResizeHorizontal,
            Self::Vertical => CursorShape::ResizeVertical,
        }
    }

    /// Splitterが制御する方向の値を得る
    const fn dominant_coordinate(&self, p: &Point<LogicalUnit>) -> f32 {
        match self {
            Self::Horizontal => p.x,
            Self::Vertical => p.y,
        }
    }
}

/// Dock間のSplitter
#[repr(transparent)]
pub struct DockedPaneSplitterView(Rc<DockedPaneSplitterEventHandler>);
impl DockedPaneSplitterView {
    /// 生成
    pub fn new(
        ctx: &mut ViewInitContext,
        dir: DockedPaneSplitDirection,
        controlling_dock: DockID,
    ) -> Self {
        let ct_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                1.0, 1.0, 1.0, 0.125,
            ])),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            cursor_shape: dir.cursor_shape(),
            ..Default::default()
        });

        let eh = Rc::new(DockedPaneSplitterEventHandler {
            view_id: ctx.view_registry.alloc(),
            dir,
            controlling_dock: Cell::new(controlling_dock),
            ct_root,
            ht_root,
            pressing: Cell::new(false),
            pending_relayout: Cell::new(None),
            drag_delta: Cell::new(0.0),
        });
        ctx.ht_manager.set_action_handler(eh.ht_root, &eh);
        ctx.view_registry.set_event_handler(eh.view_id, &eh);

        Self(eh)
    }

    /// 後始末
    fn teardown(self, ctx: &mut TeardownContext) {
        ctx.mount_context.ht_manager.remove_child(self.0.ht_root);
        ctx.mount_context
            .composite_tree
            .remove_child(self.0.ct_root);

        ctx.mount_context.ht_manager.free_all(self.0.ht_root);
        ctx.mount_context.composite_tree.free_all(self.0.ct_root);
    }

    /// マウント
    fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.0.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.0.ht_root);
    }

    /// サイズ調整
    #[inline(always)]
    fn resize<E>(
        &self,
        rect: Rect<LogicalUnit>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        self.0.perform_relayout(rect, composite_tree, ht_manager);
    }

    /// 制御対象のDockを変更
    #[inline(always)]
    fn rebind_controlling_dock(&self, dock: DockID) {
        self.0.controlling_dock.set(dock);
    }
}

/// Splitterのイベントハンドラ
struct DockedPaneSplitterEventHandler {
    /// View ID
    view_id: ViewIdentifier,
    /// 分割方向
    dir: DockedPaneSplitDirection,
    /// 制御対象のDock
    controlling_dock: Cell<DockID>,
    /// ビジュアルツリー
    ct_root: CompositeTreeRef,
    /// 入力ツリー
    ht_root: HitTestTreeRef,
    /// ポインタ押下中か？
    pressing: Cell<bool>,
    /// レイアウト適用待ちの矩形
    pending_relayout: Cell<Option<Rect<LogicalUnit>>>,
    /// ドラッグ操作のオフセット
    drag_delta: Cell<f32>,
}
impl ViewEventHandler for DockedPaneSplitterEventHandler {
    fn update(&self, context: &mut ViewUpdateContext) {
        if let Some(new_rect) = self.pending_relayout.take() {
            self.perform_relayout(
                new_rect,
                context.mount_context.composite_tree,
                context.mount_context.ht_manager,
            );
        }
    }
}
impl HitTestTreeActionHandler for DockedPaneSplitterEventHandler {
    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.drag_delta.set(match self.dir {
            DockedPaneSplitDirection::Horizontal => {
                args.client_pos.x - context.ht_manager.compute_global_rect_autoroot(sender).0
            }
            DockedPaneSplitDirection::Vertical => {
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

        self.r#move(&args.client_pos, context.system_link.event_dispatcher());
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_move(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        self.r#move(&args.client_pos, context.system_link.event_dispatcher());
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
impl DockedPaneSplitterEventHandler {
    /// 動かす
    fn r#move(
        &self,
        client_pos: &Point<PointerInputUnit>,
        event_dispatcher: &LogicFiberEventDispatcher,
    ) {
        event_dispatcher.dispatch(Event::DockMoveSplitter {
            controlling_dock: self.controlling_dock.get(),
            pos_client: self.dir.dominant_coordinate(client_pos) + self.drag_delta.get(),
        });
    }

    /// レイアウトを適用する
    fn perform_relayout<E>(
        &self,
        new_rect: Rect<LogicalUnit>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        composite_tree.get_mut(self.ct_root).offset = [
            AnimatableFloat::Value(new_rect.left),
            AnimatableFloat::Value(new_rect.top),
        ];
        composite_tree.get_mut(self.ct_root).size = [
            AnimatableFloat::Value(new_rect.width),
            AnimatableFloat::Value(new_rect.height),
        ];
        composite_tree.mark_dirty(self.ct_root);
        ht_manager.get_data_mut(self.ht_root).left = new_rect.left;
        ht_manager.get_data_mut(self.ht_root).top = new_rect.top;
        ht_manager.get_data_mut(self.ht_root).width = new_rect.width;
        ht_manager.get_data_mut(self.ht_root).height = new_rect.height;
    }
}

static PANE_GROUP_TAB_ACTIVE_GRADIENT: UnsafeMainThreadOnlyOnceCell<GradientRef> =
    UnsafeMainThreadOnlyOnceCell(std::cell::OnceCell::new());
fn pane_group_tab_active_gradient<E>(composite_tree: &mut CompositeTree<E>) -> GradientRef {
    *PANE_GROUP_TAB_ACTIVE_GRADIENT.0.get_or_init(|| {
        composite_tree.create_gradient(Gradient::Linear {
            start_color: [0.0, 0.5, 1.0, 0.0],
            end_color: [0.0, 0.75, 1.2, 1.0],
            start_pos_relative: [0.0, 0.8],
            end_pos_relative: [0.0, 1.0],
        })
    })
}

/// Paneのグループ
#[repr(transparent)]
pub struct PaneGroupView {
    /// コントローラインスタンス
    controller: Rc<PaneGroupViewController>,
}
impl PaneGroupView {
    /// 生成
    pub fn new(
        ctx: &mut ViewInitContext,
        contents: Vec<Box<dyn PaneContentPresenter>>,
        dock: DockID,
    ) -> Self {
        let ct_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            clip_child: Some(ClipConfig {
                left_softness: SafeF32::ZERO,
                top_softness: SafeF32::ZERO,
                right_softness: SafeF32::ZERO,
                bottom_softness: SafeF32::ZERO,
            }),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            ..Default::default()
        });

        let ct_content_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            relative_size_adjustment: [1.0, 1.0],
            offset: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(DESIGN_METRICS.tab_height()),
            ],
            size: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(-DESIGN_METRICS.tab_height()),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                0.15, 0.15, 0.15, 1.0,
            ])),
            ..Default::default()
        });
        let ht_content_root = ctx.ht_manager.create(HitTestTreeData {
            top: DESIGN_METRICS.tab_height(),
            height: -DESIGN_METRICS.tab_height(),
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });

        ctx.composite_tree.add_child(ct_root, ct_content_root);
        ctx.ht_manager.add_child(ht_root, ht_content_root);

        let controller = Rc::new_cyclic(|wgc| PaneGroupViewController {
            view_id: ctx.view_registry.alloc(),
            ct_root,
            ht_root,
            ct_content_root,
            ht_content_root,
            dock: Cell::new(dock),
            contents: RefCell::new(
                contents
                    .into_iter()
                    .map(|c| {
                        let tv = PaneGroupTabView::new(ctx, c.name(), wgc.clone());
                        tv.mount(ctx, &RawMountTarget { ct_root, ht_root });
                        (c, tv)
                    })
                    .collect(),
            ),
            current_active_index: Cell::new(0),
            pending_active_changes: Cell::new(None),
            pending_set_rect: Cell::new(None),
        });
        ctx.view_registry
            .set_event_handler(controller.view_id, &controller);

        Self::relocate_tabs(
            controller.contents.borrow().iter().map(|(_, t)| t),
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );
        controller.activate(0, ctx);

        Self { controller }
    }

    /// 後始末
    fn teardown(self, ctx: &mut TeardownContext) {
        ctx.mount_context
            .composite_tree
            .remove_child(self.controller.ct_root);
        ctx.mount_context
            .ht_manager
            .remove_child(self.controller.ht_root);

        ctx.mount_context
            .composite_tree
            .free_all(self.controller.ct_root);
        ctx.mount_context
            .ht_manager
            .free_all(self.controller.ht_root);

        for (mut c, t) in self.controller.contents.borrow_mut().drain(..) {
            t.teardown(ctx);
            c.teardown(ctx);
        }
    }

    /// マウント
    fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.controller.ct_root);
        ctx.ht_manager
            .add_child(target.ht_root(), self.controller.ht_root);
    }

    /// 矩形を設定する
    #[inline(always)]
    pub fn set_rect(
        &self,
        rect: Rect<LogicalUnit>,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        self.controller
            .perform_set_rect(rect, composite_tree, ht_manager);
    }

    /// このグループが乗っているDockを変更する
    #[inline(always)]
    fn rebind_dock(&self, dock: DockID) {
        self.controller.dock.set(dock);
    }

    /// タブとのヒットテストを行い、ヒットしたタブのインデックス番号と左上の座標を返す
    fn hittest_tab_index(&self, pos: Point<LogicalUnit>) -> (usize, Point<LogicalUnit>) {
        if pos.x < 0.0 {
            return (0, Point::new_logical(0.0, 0.0));
        }

        let mut leftmost = 0.0;
        for (index, (_, tv)) in self.controller.contents.borrow().iter().enumerate() {
            if leftmost <= pos.x && pos.x <= leftmost + tv.size.width {
                return (index, Point::new_logical(leftmost, 0.0));
            }
            leftmost += tv.size.width;
        }

        (
            self.controller.contents.borrow().len(),
            Point::new_logical(leftmost, 0.0),
        )
    }

    /// コンテンツを追加する
    fn add_content(
        &self,
        content: Box<dyn PaneContentPresenter>,
        ctx: &mut ViewInitContext,
        with_activate: bool,
    ) {
        let tab_view = PaneGroupTabView::new(ctx, content.name(), Rc::downgrade(&self.controller));
        tab_view.mount(
            ctx,
            &RawMountTarget {
                ct_root: self.controller.ct_root,
                ht_root: self.controller.ht_root,
            },
        );
        self.controller
            .contents
            .borrow_mut()
            .push((content, tab_view));
        Self::relocate_tabs(
            self.controller.contents.borrow().iter().map(|(_, t)| t),
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );

        if with_activate {
            self.controller
                .perform_change_active(self.controller.contents.borrow().len() - 1, ctx);
        }
    }

    /// コンテンツを挿入する
    fn insert_content(
        &self,
        content: Box<dyn PaneContentPresenter>,
        index: usize,
        ctx: &mut ViewInitContext,
        with_activate: bool,
    ) {
        let tab_view = PaneGroupTabView::new(ctx, content.name(), Rc::downgrade(&self.controller));
        tab_view.mount(
            ctx,
            &RawMountTarget {
                ct_root: self.controller.ct_root,
                ht_root: self.controller.ht_root,
            },
        );
        self.controller
            .contents
            .borrow_mut()
            .insert(index, (content, tab_view));
        self.controller
            .current_active_index
            .update(|x| if x >= index { x + 1 } else { x });
        Self::relocate_tabs(
            self.controller.contents.borrow().iter().map(|(_, t)| t),
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );

        if with_activate {
            self.controller.perform_change_active(index, ctx);
        }
    }

    /// コンテンツを削除する
    fn remove_content(
        &self,
        index: usize,
        ctx: &mut TeardownContext,
    ) -> Box<dyn PaneContentPresenter> {
        let is_active = self.controller.current_active_index.get() == index;
        let (content, tab) = self.controller.contents.borrow_mut().remove(index);
        if is_active {
            content.unmount(&mut ctx.mount_context);

            if !self.controller.contents.borrow().is_empty() {
                let new_active = self
                    .controller
                    .current_active_index
                    .get()
                    .clamp(0, self.controller.contents.borrow().len() - 1);
                self.controller.activate(new_active, &mut ctx.mount_context);
                self.controller.current_active_index.set(new_active);
            }
        }
        // TODO: ここのアクティブインデックスのメンテ処理が不十分なので直す（最後のタブがアクティブのときに他のタブをグループから外すと次に切り替えしたときに範囲外参照になる）
        // アクティブタブをインデックスで持つんじゃなくてインスタンスアドレスで持つ形にするのがよさそうかも

        Self::relocate_tabs(
            self.controller.contents.borrow().iter().map(|(_, t)| t),
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );

        tab.teardown(ctx);
        content
    }

    /// コンテンツが一つ以上存在するかどうかを返す
    #[inline(always)]
    fn has_contents(&self) -> bool {
        !self.controller.contents.borrow().is_empty()
    }

    /// タブを再配置する
    fn relocate_tabs<'t, E>(
        tabs: impl Iterator<Item = &'t PaneGroupTabView>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        let mut left_offset = 0.0;
        for t in tabs {
            t.place(
                Point::new_logical(left_offset, 0.0),
                composite_tree,
                ht_manager,
            );
            left_offset += t.size.width;
        }
    }
}

/// PaneGroupViewのコントローラ
struct PaneGroupViewController {
    /// View ID
    view_id: ViewIdentifier,
    /// ビジュアルツリー ルート
    ct_root: CompositeTreeRef,
    /// 入力ツリー ルート
    ht_root: HitTestTreeRef,
    /// コンテンツのビジュアルツリー ルート
    ct_content_root: CompositeTreeRef,
    /// コンテンツの入力ツリー ルート
    ht_content_root: HitTestTreeRef,
    /// 所属Dock
    dock: Cell<DockID>,
    /// 内容物とそのタブViewのリスト
    contents: RefCell<Vec<(Box<dyn PaneContentPresenter>, PaneGroupTabView)>>,
    /// 現在アクティブなコンテンツのインデックス
    current_active_index: Cell<usize>,
    /// アクティブなコンテンツの変更待ちインデックス
    pending_active_changes: Cell<Option<usize>>,
    /// コンテンツのリサイズ待ち情報
    pending_set_rect: Cell<Option<Rect<LogicalUnit>>>,
}
impl ViewEventHandler for PaneGroupViewController {
    fn update(&self, context: &mut ViewUpdateContext) {
        if let Some(index) = self.pending_active_changes.take() {
            self.perform_change_active(index, context);
        }

        if let Some(rect) = self.pending_set_rect.take() {
            self.perform_set_rect(
                rect,
                context.mount_context.composite_tree,
                context.mount_context.ht_manager,
            );
        }
    }
}
impl PaneGroupViewController {
    /// タブViewのインスタンス参照からインデックスを計算する
    #[inline(always)]
    fn tab_index(&self, tab: &PaneGroupTabEventHandler) -> Option<usize> {
        self.contents
            .borrow()
            .iter()
            .position(|x| core::ptr::addr_eq(x.1.as_ref(), tab))
    }

    /// タブを選択する
    fn select_tab(&self, tab: &PaneGroupTabEventHandler, e: &LogicFiberEventDispatcher) {
        let Some(index) = self.tab_index(tab) else {
            tracing::warn!("no tab found");
            return;
        };

        self.pending_active_changes.set(Some(index));
        e.dispatch(Event::UpdateView { id: self.view_id });
    }

    /// アクティブなコンテンツを変更する
    fn perform_change_active(&self, new_active_index: usize, ctx: &mut MountContext) {
        let old_active = self.current_active_index.replace(new_active_index);
        if old_active != new_active_index {
            let old_active = &self.contents.borrow()[old_active];
            old_active.0.unmount(ctx);
            old_active
                .1
                .set_active(false, ctx.composite_tree, ctx.current_sec);

            self.activate(new_active_index, ctx);
        }
    }

    /// コンテンツをアクティブ状態にする
    fn activate(&self, index: usize, context: &mut MountContext) {
        let target = &self.contents.borrow()[index];
        target.0.mount(
            context,
            &RawMountTarget {
                ct_root: self.ct_content_root,
                ht_root: self.ht_content_root,
            },
        );
        target
            .1
            .set_active(true, context.composite_tree, context.current_sec);
    }

    /// コンテンツのリサイズを実行する
    fn perform_set_rect(
        &self,
        rect: Rect<LogicalUnit>,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        composite_tree.get_mut(self.ct_root).offset = [
            AnimatableFloat::Value(rect.left),
            AnimatableFloat::Value(rect.top),
        ];
        composite_tree.get_mut(self.ct_root).size = [
            AnimatableFloat::Value(rect.width),
            AnimatableFloat::Value(rect.height),
        ];
        composite_tree.mark_dirty(self.ct_root);
        ht_manager.get_data_mut(self.ht_root).left = rect.left;
        ht_manager.get_data_mut(self.ht_root).top = rect.top;
        ht_manager.get_data_mut(self.ht_root).width = rect.width;
        ht_manager.get_data_mut(self.ht_root).height = rect.height;

        let content_size = Size::new_logical(rect.width, rect.height - DESIGN_METRICS.tab_height());
        for (c, _) in self.contents.borrow().iter() {
            c.resize(&content_size, composite_tree, ht_manager);
        }
    }
}

/// タブView
#[repr(transparent)]
struct PaneGroupTabView(Rc<PaneGroupTabEventHandler>);
impl core::ops::Deref for PaneGroupTabView {
    type Target = Rc<PaneGroupTabEventHandler>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl PaneGroupTabView {
    /// 生成
    fn new(
        ctx: &mut ViewInitContext,
        label: String,
        group_controller: std::rc::Weak<PaneGroupViewController>,
    ) -> Self {
        let active_gradient = pane_group_tab_active_gradient(ctx.composite_tree);
        let tw =
            TextLayout::measure_visual_width(&label, FontID::UIDefault, ctx.system_link.font_set());
        let size = Size::new_logical(
            tw + DESIGN_METRICS.tab_padding_x * 2.0,
            DESIGN_METRICS.tab_height(),
        );

        let ct_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            size: [
                AnimatableFloat::Value(size.width),
                AnimatableFloat::Value(size.height),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
            corner_radius: CornerRadius::all(DESIGN_METRICS.tab_rounding),
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
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
        let ct_active = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            relative_size_adjustment: [1.0, 1.0],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
            corner_radius: CornerRadius::all(DESIGN_METRICS.tab_rounding),
            ..Default::default()
        });
        let ct_underline = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            relative_size_adjustment: [1.0, 1.0],
            relative_offset_adjustment: [0.0, 0.0],
            has_bitmap: true,
            composite_mode: CompositeMode::FillLinearGradient(active_gradient),
            corner_radius: CornerRadius::all(DESIGN_METRICS.tab_rounding),
            scale_x: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            width: size.width,
            height: size.height,
            cursor_shape: CursorShape::Pointer,
            ..Default::default()
        });

        ctx.composite_tree.add_child(ct_root, ct_active);
        ctx.composite_tree.add_child(ct_root, ct_underline);

        let eh = Rc::new(PaneGroupTabEventHandler {
            ct_root,
            ct_active,
            ct_underline,
            ht_root,
            size,
            active: Cell::new(false),
            group_controller,
        });
        ctx.ht_manager.set_action_handler(ht_root, &eh);

        Self(eh)
    }

    /// 後始末
    fn teardown(self, ctx: &mut TeardownContext) {
        ctx.mount_context
            .composite_tree
            .remove_child(self.0.ct_root);
        ctx.mount_context.ht_manager.remove_child(self.ht_root);

        ctx.mount_context.composite_tree.free_all(self.0.ct_root);
        ctx.mount_context.ht_manager.free_all(self.ht_root);
    }

    /// マウント
    fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.0.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.ht_root);
    }

    /// 配置
    fn place<E>(
        &self,
        pos: Point<LogicalUnit>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        composite_tree.get_mut(self.0.ct_root).offset =
            [AnimatableFloat::Value(pos.x), AnimatableFloat::Value(pos.y)];
        composite_tree.mark_dirty(self.0.ct_root);
        ht_manager.get_data_mut(self.ht_root).left = pos.x;
        ht_manager.get_data_mut(self.ht_root).top = pos.y;
    }
}

/// タブViewのイベントハンドラ
struct PaneGroupTabEventHandler {
    /// ビジュアルツリー ルート
    ct_root: CompositeTreeRef,
    /// ビジュアルツリー アクティブ表示
    ct_active: CompositeTreeRef,
    /// ビジュアルツリー 下線
    ct_underline: CompositeTreeRef,
    /// 入力ツリー ルート
    ht_root: HitTestTreeRef,
    /// 大きさ
    size: Size<LogicalUnit>,
    /// アクティブ状態か？
    active: Cell<bool>,
    /// 所属するPaneGroupViewControllerの弱参照
    group_controller: std::rc::Weak<PaneGroupViewController>,
}
impl HitTestTreeActionHandler for PaneGroupTabEventHandler {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        context.composite_tree.get_mut(self.ct_root).composite_mode =
            CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.25],
                start_sec: context.current_sec,
                end_sec: context.current_sec + 0.1,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            });
        context.composite_tree.mark_dirty(self.ct_root);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        context.composite_tree.get_mut(self.ct_root).composite_mode =
            CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.25],
                to_value: [1.0, 1.0, 1.0, 0.0],
                start_sec: context.current_sec,
                end_sec: context.current_sec + 0.1,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            });
        context.composite_tree.mark_dirty(self.ct_root);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if args.button == PointerButton::Primary {
            if let Some(gc) = self.group_controller.upgrade() {
                gc.select_tab(self, context.system_link.event_dispatcher());
            }
        } else {
            context.system_link.dispatch_event(Event::MenuOpen {
                parent: context
                    .ht_manager
                    .query_root_window(sender)
                    .expect("not mounted"),
                items: vec![
                    crate::uikit::MenuItem::Command {
                        label: "Entry1".into(),
                        command_id: 0,
                    },
                    crate::uikit::MenuItem::Command {
                        label: "Entry2".into(),
                        command_id: 1,
                    },
                    crate::uikit::MenuItem::Separator,
                    crate::uikit::MenuItem::Command {
                        label: "Entry3".into(),
                        command_id: 2,
                    },
                    crate::uikit::MenuItem::Heading {
                        label: "Head".into(),
                    },
                    crate::uikit::MenuItem::SubMenu {
                        label: "Sub".into(),
                        items: vec![crate::uikit::MenuItem::Command {
                            label: "SubEntry1".into(),
                            command_id: 4,
                        }],
                    },
                    crate::uikit::MenuItem::Command {
                        label: "Entry4".into(),
                        command_id: 3,
                    },
                ],
                surface_pos: args.client_pos,
            });
        }

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_start(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if args.button != PointerButton::Primary {
            return EventContinueControl::empty();
        }

        if let Some(gc) = self.group_controller.upgrade() {
            let (x, y, w, h, _) = context.ht_manager.compute_global_rect_autoroot(gc.ht_root);

            context.system_link.dispatch_event(Event::DockBeginPreview {
                initiator: context
                    .ht_manager
                    .query_root_window(gc.ht_root)
                    .expect("not mounted"),
                pointer: args.pointer_id,
                pane_rect: Rect::from_lt_size(Point::new_logical(x, y), Size::new_logical(w, h)),
                tab_size: self.size.clone(),
                client_pos: args.client_pos,
                source_dock: gc.dock.get(),
                tab_index: gc.tab_index(self).expect("not in any group"),
            });
        }

        EventContinueControl::STOP_PROPAGATION
    }
}
impl PaneGroupTabEventHandler {
    /// アクティブ状態の切り替え
    fn set_active<E>(&self, active: bool, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.active.replace(active) == active {
            // active not changed
            return;
        }

        if active {
            composite_tree.get_mut(self.ct_underline).scale_x = AnimatableFloat::Animated {
                from_value: 0.0,
                to_value: 1.0,
                start_sec: current_sec,
                end_sec: current_sec + 0.2,
                curve: AnimationCurve::EASE_OUT_HARD,
                event_on_complete: None,
            };
            composite_tree.get_mut(self.ct_underline).opacity = AnimatableFloat::Animated {
                from_value: 0.0,
                to_value: 1.0,
                start_sec: current_sec,
                end_sec: current_sec + 0.1,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            };
            composite_tree.mark_dirty(self.ct_underline);

            composite_tree.get_mut(self.ct_active).composite_mode =
                CompositeMode::FillColor(AnimatableColor::Animated {
                    from_value: [1.0, 1.0, 1.0, 0.0],
                    to_value: [1.0, 1.0, 1.0, 0.1],
                    start_sec: current_sec,
                    end_sec: current_sec + 0.2,
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                });
            composite_tree.mark_dirty(self.ct_active);
        } else {
            composite_tree.get_mut(self.ct_underline).scale_x = AnimatableFloat::Animated {
                from_value: 1.0,
                to_value: 0.0,
                start_sec: current_sec,
                end_sec: current_sec + 0.2,
                curve: AnimationCurve::EASE_IN,
                event_on_complete: None,
            };
            composite_tree.mark_dirty(self.ct_underline);

            composite_tree.get_mut(self.ct_active).composite_mode =
                CompositeMode::FillColor(AnimatableColor::Animated {
                    from_value: [1.0, 1.0, 1.0, 0.1],
                    to_value: [1.0, 1.0, 1.0, 0.0],
                    start_sec: current_sec,
                    end_sec: current_sec + 0.2,
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                });
            composite_tree.mark_dirty(self.ct_active);
        }
    }
}
