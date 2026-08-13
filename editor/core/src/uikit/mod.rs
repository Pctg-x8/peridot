//! Non-Application related common ui kits

use std::{
    collections::{BTreeSet, HashMap, HashSet, VecDeque},
    num::NonZeroUsize,
    rc::{Rc, Weak},
};

use crate::{
    Application, SyncEvent, SystemLink,
    input::{
        FocusTargetToken, KeyboardFocusGroupRef, KeyboardFocusTokenRegistry,
        hittest::{HitTestTreeManager, HitTestTreeRef},
    },
    rendering::{
        MainThreadTextureIDIssuer,
        composite::{CompositeTree, CompositeTreeRef},
    },
    utils::{LogicalUnit, Point, Size},
};

pub trait SystemLinkAccess {
    fn system_link<'a>(&'a self) -> &'a SystemLink<'a>;
}

pub trait CompositeTreeMutableAccess<Event> {
    fn composite_tree_mut(&mut self) -> &mut CompositeTree<Event>;
}

pub trait HitTestTreeMutableAccess<'h> {
    fn hit_test_tree_mut(&mut self) -> &mut HitTestTreeManager<'h>;
}

pub struct MountContext<'a, 'h> {
    pub composite_tree: &'a mut CompositeTree<SyncEvent>,
    pub ht_manager: &'a mut HitTestTreeManager<'h>,
    pub keyboard_focus_registry: &'a mut KeyboardFocusTokenRegistry,
    pub current_sec: f32,
}

pub trait DeriveMountContext<'h> {
    fn derive_mount_context<'env2>(&'env2 mut self) -> MountContext<'env2, 'h>;
}

pub struct RenderContext<'env, 'h> {
    pub composite_tree: &'env mut CompositeTree<SyncEvent>,
    pub ht_manager: &'env mut HitTestTreeManager<'h>,
    pub keyboard_focus_registry: &'env mut KeyboardFocusTokenRegistry,
    pub current_sec: f32,
    pub system_link: &'env SystemLink<'env>,
    pub main_thread_texture_id_issuer: &'env mut MainThreadTextureIDIssuer,
    pub application: &'env Application,
    pub view_feedback_subscription_delayed_ops: &'env mut VecDeque<ViewFeedbackRegistryDelayedOps>,
}
impl<'h> RenderContext<'_, 'h> {
    pub const fn make_mount_context<'env>(&'env mut self) -> MountContext<'env, 'h> {
        MountContext {
            composite_tree: self.composite_tree,
            ht_manager: self.ht_manager,
            keyboard_focus_registry: self.keyboard_focus_registry,
            current_sec: self.current_sec,
        }
    }

    pub fn subscribe_view_feedback<T: 'static>(
        &mut self,
        handler: &Rc<impl ViewFeedbackHandler<T> + 'static>,
    ) {
        self.view_feedback_subscription_delayed_ops
            .push_back(ViewFeedbackRegistryDelayedOps::subscribe(handler));
    }

    pub fn unsubscribe_view_feedback<T: 'static>(
        &mut self,
        handler: &Rc<impl ViewFeedbackHandler<T> + 'static>,
    ) {
        self.view_feedback_subscription_delayed_ops
            .push_back(ViewFeedbackRegistryDelayedOps::unsubscribe(handler));
    }
}

pub struct RenderChildScheduler {
    mount_on: Option<RawMountTarget>,
}
impl RenderChildScheduler {
    pub fn new() -> Self {
        Self { mount_on: None }
    }

    pub fn schedule_render_children(&mut self, mount_on: RawMountTarget) {
        self.mount_on = Some(mount_on);
    }
}

pub struct ViewInitContext<'a, 'h> {
    pub mount_context: MountContext<'a, 'h>,
    pub view_allocator: &'a mut ViewIdentifierAllocator,
    pub view_instance_store: &'a mut ViewInstanceStore,
    pub view_tree_relation_store: &'a mut ViewTreeRelationStore,
    pub view_event_handler_store: &'a mut ViewEventHandlerStore,
    pub view_group_relation_store: &'a mut ViewGroupRelationStore,
    pub view_render_state_store: &'a mut ViewRenderStateStore,
    pub view_feedback_subscription_delayed_ops: &'a mut VecDeque<ViewFeedbackRegistryDelayedOps>,
    pub system_link: &'a SystemLink<'a>,
    pub main_thread_texture_id_issuer: &'a mut MainThreadTextureIDIssuer,
    pub application: &'a Application,
}
impl<'a, 'h> core::ops::Deref for ViewInitContext<'a, 'h> {
    type Target = MountContext<'a, 'h>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.mount_context
    }
}
impl<'a, 'h> core::ops::DerefMut for ViewInitContext<'a, 'h> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mount_context
    }
}
impl ViewRegisterable for ViewInitContext<'_, '_> {
    fn construct_view(
        &mut self,
        ctor: impl FnOnce(ViewIdentifier) -> Box<dyn View>,
    ) -> ViewIdentifier {
        construct_view(
            ctor,
            self.view_allocator,
            self.view_instance_store,
            self.view_event_handler_store,
            self.view_tree_relation_store,
            self.view_group_relation_store,
            self.view_render_state_store,
        )
    }

    fn free_view(&mut self, id: ViewIdentifier) {
        free_view(
            id,
            self.view_allocator,
            self.view_instance_store,
            self.view_event_handler_store,
            self.view_tree_relation_store,
            self.view_group_relation_store,
            self.view_render_state_store,
        )
    }
}
impl ViewRelationControllable for ViewInitContext<'_, '_> {
    fn view_set_parent(&mut self, id: ViewIdentifier, parent: ViewIdentifier) {
        view_set_parent(id, parent, self.view_tree_relation_store)
    }

    fn view_detach_parent(&mut self, id: ViewIdentifier) {
        view_detach_parent(id, self.view_tree_relation_store);
    }
}
impl ViewInstanceQueryable for ViewInitContext<'_, '_> {
    #[inline(always)]
    fn view_instance<T: View + 'static>(&self, id: ViewIdentifier) -> Option<&T> {
        view_instance(id, self.view_instance_store)
    }
}
impl ViewInstanceQueryableMut for ViewInitContext<'_, '_> {
    #[inline(always)]
    fn view_instance_mut<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T> {
        view_instance_mut(id, self.view_instance_store)
    }

    #[inline(always)]
    fn view_set_visibility(&mut self, id: ViewIdentifier, visible: bool) {
        crate::uikit::view_set_visibility(id, visible, self.view_instance_store);
    }
}
impl ViewImmediateRenderable for ViewInitContext<'_, '_> {
    fn render_view_recursive(
        &mut self,
        id: ViewIdentifier,
        mount_on: &(impl MountTarget + ?Sized),
        keyboard_focus_group: KeyboardFocusGroupRef,
    ) {
        render_view_recursive(
            id,
            &mut RenderContext {
                composite_tree: &mut self.mount_context.composite_tree,
                ht_manager: &mut self.mount_context.ht_manager,
                keyboard_focus_registry: &mut self.mount_context.keyboard_focus_registry,
                current_sec: self.mount_context.current_sec,
                system_link: self.system_link,
                main_thread_texture_id_issuer: self.main_thread_texture_id_issuer,
                application: self.application,
                view_feedback_subscription_delayed_ops: self.view_feedback_subscription_delayed_ops,
            },
            mount_on,
            keyboard_focus_group,
            self.view_instance_store,
            self.view_event_handler_store,
            self.view_tree_relation_store,
            self.view_render_state_store,
        )
    }
}
impl<'a, 'h> ViewInitContext<'a, 'h> {
    #[deprecated = "use render-teardown based view lifecycle"]
    pub fn alloc_view_id_without_instance(&mut self) -> ViewIdentifier {
        alloc_view_id_without_instance(
            self.view_allocator,
            self.view_instance_store,
            self.view_event_handler_store,
            self.view_tree_relation_store,
            self.view_group_relation_store,
            self.view_render_state_store,
        )
    }

    #[inline(always)]
    pub fn alloc_view_group(&mut self) -> ViewGroupID {
        alloc_view_group(self.view_allocator, self.view_group_relation_store)
    }

    #[inline(always)]
    pub fn join_view_group(&mut self, id: ViewIdentifier, group: ViewGroupID) {
        join_view_group(id, group, self.view_group_relation_store)
    }

    pub fn set_view_event_handler(
        &mut self,
        id: ViewIdentifier,
        handler: &Rc<impl ViewEventHandler + 'static>,
    ) {
        set_view_event_handler(id, handler, self.view_event_handler_store)
    }

    pub const fn make_teardown_context<'a2>(&'a2 mut self) -> TeardownContext<'a2, 'h> {
        TeardownContext {
            mount_context: MountContext {
                composite_tree: &mut self.mount_context.composite_tree,
                ht_manager: &mut self.mount_context.ht_manager,
                keyboard_focus_registry: &mut self.mount_context.keyboard_focus_registry,
                current_sec: self.mount_context.current_sec,
            },
            view_feedback_subscription_delayed_ops: &mut self
                .view_feedback_subscription_delayed_ops,
        }
    }

    pub const fn make_render_context<'env>(&'env mut self) -> RenderContext<'env, 'h> {
        RenderContext {
            composite_tree: &mut self.mount_context.composite_tree,
            ht_manager: &mut self.mount_context.ht_manager,
            keyboard_focus_registry: &mut self.mount_context.keyboard_focus_registry,
            current_sec: self.mount_context.current_sec,
            system_link: self.system_link,
            main_thread_texture_id_issuer: self.main_thread_texture_id_issuer,
            application: self.application,
            view_feedback_subscription_delayed_ops: self.view_feedback_subscription_delayed_ops,
        }
    }

    pub const fn derive<'a2>(&'a2 mut self) -> ViewInitContext<'a2, 'h> {
        ViewInitContext {
            mount_context: MountContext {
                composite_tree: &mut self.mount_context.composite_tree,
                ht_manager: &mut self.mount_context.ht_manager,
                keyboard_focus_registry: &mut self.mount_context.keyboard_focus_registry,
                current_sec: self.mount_context.current_sec,
            },
            view_allocator: &mut self.view_allocator,
            view_instance_store: &mut self.view_instance_store,
            view_tree_relation_store: &mut self.view_tree_relation_store,
            view_event_handler_store: &mut self.view_event_handler_store,
            view_group_relation_store: &mut self.view_group_relation_store,
            view_render_state_store: &mut self.view_render_state_store,
            view_feedback_subscription_delayed_ops: &mut self
                .view_feedback_subscription_delayed_ops,
            system_link: self.system_link,
            main_thread_texture_id_issuer: self.main_thread_texture_id_issuer,
            application: self.application,
        }
    }

    pub fn subscribe_view_feedback<T: 'static>(
        &mut self,
        handler: &Rc<impl ViewFeedbackHandler<T> + 'static>,
    ) {
        self.view_feedback_subscription_delayed_ops
            .push_back(ViewFeedbackRegistryDelayedOps::subscribe(handler));
    }

    pub fn unsubscribe_view_feedback<T: 'static>(
        &mut self,
        handler: &Rc<impl ViewFeedbackHandler<T> + 'static>,
    ) {
        self.view_feedback_subscription_delayed_ops
            .push_back(ViewFeedbackRegistryDelayedOps::unsubscribe(handler));
    }
}

pub struct ViewUpdateContext<'a, 'h> {
    pub mount_context: MountContext<'a, 'h>,
    pub view_instance_store: &'a mut ViewInstanceStore,
    pub view_render_queue: &'a mut ViewRenderQueue,
    pub system_link: &'a SystemLink<'a>,
}
impl<'a, 'h> core::ops::Deref for ViewUpdateContext<'a, 'h> {
    type Target = MountContext<'a, 'h>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.mount_context
    }
}
impl<'a, 'h> core::ops::DerefMut for ViewUpdateContext<'a, 'h> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mount_context
    }
}
impl ViewInstanceQueryable for ViewUpdateContext<'_, '_> {
    #[inline(always)]
    fn view_instance<T: View + 'static>(&self, id: ViewIdentifier) -> Option<&T> {
        view_instance(id, self.view_instance_store)
    }
}
impl ViewInstanceQueryableMut for ViewUpdateContext<'_, '_> {
    #[inline(always)]
    fn view_instance_mut<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T> {
        view_instance_mut(id, self.view_instance_store)
    }

    #[inline(always)]
    fn view_set_visibility(&mut self, id: ViewIdentifier, visible: bool) {
        crate::uikit::view_set_visibility(id, visible, self.view_instance_store);
    }
}
impl ViewRenderer for ViewUpdateContext<'_, '_> {
    #[inline(always)]
    fn schedule_view_render(&mut self, target: ViewIdentifier) {
        self.view_render_queue.schedule(target);
    }
}
impl<'h> DeriveMountContext<'h> for ViewUpdateContext<'_, 'h> {
    fn derive_mount_context<'env2>(&'env2 mut self) -> MountContext<'env2, 'h> {
        MountContext {
            composite_tree: &mut self.mount_context.composite_tree,
            ht_manager: &mut self.mount_context.ht_manager,
            keyboard_focus_registry: &mut self.mount_context.keyboard_focus_registry,
            current_sec: self.mount_context.current_sec,
        }
    }
}

pub struct TeardownContext<'a, 'h> {
    pub mount_context: MountContext<'a, 'h>,
    pub view_feedback_subscription_delayed_ops: &'a mut VecDeque<ViewFeedbackRegistryDelayedOps>,
}
impl<'a, 'h> TeardownContext<'a, 'h> {
    pub fn subscribe_view_feedback<T: 'static>(
        &mut self,
        handler: &Rc<impl ViewFeedbackHandler<T> + 'static>,
    ) {
        self.view_feedback_subscription_delayed_ops
            .push_back(ViewFeedbackRegistryDelayedOps::subscribe(handler));
    }

    pub fn unsubscribe_view_feedback<T: 'static>(
        &mut self,
        handler: &Rc<impl ViewFeedbackHandler<T> + 'static>,
    ) {
        self.view_feedback_subscription_delayed_ops
            .push_back(ViewFeedbackRegistryDelayedOps::unsubscribe(handler));
    }
}

pub trait DeriveTeardownContext<'h> {
    fn derive_teardown_context<'env>(&'env mut self) -> TeardownContext<'env, 'h>;
}

pub trait MountTarget {
    fn ct_root(&self) -> CompositeTreeRef;
    fn ht_root(&self) -> HitTestTreeRef;
}
impl<T> MountTarget for &'_ T
where
    T: MountTarget + ?Sized,
{
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        T::ct_root(self)
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        T::ht_root(self)
    }
}

#[derive(Clone)]
pub struct RawMountTarget {
    pub ct_root: CompositeTreeRef,
    pub ht_root: HitTestTreeRef,
}
impl MountTarget for RawMountTarget {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        self.ct_root
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        self.ht_root
    }
}
impl RawMountTarget {
    #[inline(always)]
    pub fn from_typed(target: &(impl MountTarget + ?Sized)) -> Self {
        Self {
            ct_root: target.ct_root(),
            ht_root: target.ht_root(),
        }
    }
}

/// Viewの位置の指定 デフォルトでは親コンテナに対して(0, 0)に配置される
#[derive(Clone)]
pub struct ViewLocation {
    /// 親コンテナ内での基準となる(大きさに対する)相対位置
    pub parent_anchor: [f32; 2],
    /// 自身のサイズに対する相対位置([1.0, 0,0]を指定すると右橋基準で配置される)
    pub anchor: [f32; 2],
    /// オフセット量
    pub offset: Point<LogicalUnit>,
}
impl Default for ViewLocation {
    fn default() -> Self {
        Self {
            parent_anchor: [0.0; 2],
            anchor: [0.0; 2],
            offset: Point::new_logical(0.0, 0.0),
        }
    }
}
impl ViewLocation {
    pub const fn new_left_top(x: f32, y: f32) -> Self {
        Self {
            parent_anchor: [0.0, 0.0],
            anchor: [0.0, 0.0],
            offset: Point::new_logical(x, y),
        }
    }

    pub const fn compute(&self, size: &Size<LogicalUnit>) -> Point<LogicalUnit> {
        Point::new_logical(
            self.offset.x - size.width * self.anchor[0],
            self.offset.y - size.height * self.anchor[1],
        )
    }
}

#[derive(Clone)]
pub enum ViewElementSize {
    Automatic,
    Fixed(Size<LogicalUnit>),
}
impl ViewElementSize {
    pub const fn fixed(width: f32, height: f32) -> Self {
        Self::Fixed(Size::new_logical(width, height))
    }
}

#[derive(Clone)]
pub struct ViewPlacement {
    pub location: ViewLocation,
    pub size: ViewElementSize,
    pub size_anchor: [f32; 2],
}
impl Default for ViewPlacement {
    fn default() -> Self {
        Self {
            location: ViewLocation::new_left_top(0.0, 0.0),
            size: ViewElementSize::Automatic,
            size_anchor: [0.0; 2],
        }
    }
}
impl ViewPlacement {
    /// 実際に配置されるサイズを計算する 引数にはAutomatic指定時の算出ロジックを渡す(Viewによって異なる)
    pub fn actual_size(&self, automatic: impl FnOnce() -> Size<LogicalUnit>) -> Size<LogicalUnit> {
        match self.size {
            ViewElementSize::Fixed(x) => x,
            ViewElementSize::Automatic => automatic(),
        }
    }

    /// 実際に配置されるオフセット位置を計算する
    pub fn actual_offset(&self, actual_size: &Size<LogicalUnit>) -> Point<LogicalUnit> {
        Point::new_logical(
            self.location.offset.x - actual_size.width * self.location.anchor[0],
            self.location.offset.y - actual_size.height * self.location.anchor[1],
        )
    }
}

#[derive(Default)]
pub struct ViewNewRenderElements {
    pub composite_tree: Option<CompositeTreeRef>,
    pub hit_tree: Option<HitTestTreeRef>,
    pub keyboard_focus: Option<FocusTargetToken>,
}
impl ViewNewRenderElements {
    pub const EMPTY: Self = Self {
        composite_tree: None,
        hit_tree: None,
        keyboard_focus: None,
    };

    pub fn mount_on(
        &self,
        target: &(impl MountTarget + ?Sized),
        kf_group: KeyboardFocusGroupRef,
        ctx: &mut MountContext,
    ) {
        if let Some(composite_tree) = self.composite_tree {
            ctx.composite_tree
                .add_child(target.ct_root(), composite_tree);
        }
        if let Some(hit_tree) = self.hit_tree {
            ctx.ht_manager.add_child(target.ht_root(), hit_tree);
        }
        if let Some(keyboard_focus) = self.keyboard_focus {
            ctx.keyboard_focus_registry
                .join_group(kf_group, keyboard_focus);
        }
    }
}

/// Viewのライフサイクル
pub trait View: core::any::Any {
    /// Render(初回マウント/更新)時に呼ばれる
    fn render(
        &mut self,
        self_instance: &mut ViewInstanceModifier,
        ctx: &mut RenderContext,
        sched: &mut RenderChildScheduler,
    ) -> ViewNewRenderElements;

    /// Teardown(アンマウント)時に呼ばれる
    fn teardown(&mut self, ctx: &mut TeardownContext);
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ViewIdentifier(NonZeroUsize);
impl core::fmt::Debug for ViewIdentifier {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "View#{}", self.0)
    }
}
impl ViewIdentifier {
    const fn into_array_index(self) -> usize {
        self.0.get() - 1
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ViewGroupID(NonZeroUsize);
impl core::fmt::Debug for ViewGroupID {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ViewGroup#{}", self.0)
    }
}
impl ViewGroupID {
    const fn into_array_index(self) -> usize {
        self.0.get() - 1
    }
}

pub struct ViewInstanceModifier<'a> {
    event_handler_ref: &'a mut Weak<dyn ViewEventHandler>,
}
impl ViewInstanceModifier<'_> {
    #[inline(always)]
    pub fn bind_event_handler(&mut self, handler: &std::rc::Rc<impl ViewEventHandler + 'static>) {
        *self.event_handler_ref = Rc::downgrade(handler) as _;
    }
}

pub struct ViewRenderQueue {
    pending: BTreeSet<ViewIdentifier>,
}
impl ViewRenderQueue {
    pub fn new() -> Self {
        Self {
            pending: BTreeSet::new(),
        }
    }

    pub fn schedule(&mut self, id: ViewIdentifier) {
        self.pending.insert(id);
    }

    pub fn perform(
        &mut self,
        ctx: &mut RenderContext,
        instance_store: &mut ViewInstanceStore,
        tree_relation_store: &ViewTreeRelationStore,
        render_state_store: &mut ViewRenderStateStore,
        event_handler_store: &mut ViewEventHandlerStore,
    ) {
        while let Some(mut target) = self.pending.pop_first() {
            let (mount_target, kf_group) = loop {
                let Some(p) = tree_relation_store.relations[target.into_array_index()].parent
                else {
                    // root
                    match render_state_store.0[target.into_array_index()].current_mounted_on {
                        None => {
                            panic!("unable to re-render root, which haven't rendered yet");
                        }
                        Some((ref mt, kfg)) => break (mt.clone(), kfg),
                    }
                };

                if self.pending.contains(&p) {
                    // 親も更新対象
                    self.pending.remove(&p);
                    target = p;
                    continue;
                }

                match render_state_store.0[target.into_array_index()].current_mounted_on {
                    None => {
                        // 親がわからないので親からrenderする
                        target = p;
                    }
                    Some((ref mt, kfg)) => break (mt.clone(), kfg),
                }
            };

            let mut scheduled_renders = VecDeque::new();
            scheduled_renders.push_back((mount_target, target));
            while let Some((mt, v)) = scheduled_renders.pop_front() {
                let mut sched = RenderChildScheduler::new();
                render_view_instance1(
                    v,
                    ctx,
                    &mut sched,
                    mt,
                    kf_group,
                    instance_store,
                    event_handler_store,
                    render_state_store,
                );

                // もうrenderしたので次のループからはRenderしない
                self.pending.remove(&v);
                if let Some(mt) = sched.mount_on {
                    scheduled_renders.extend(
                        tree_relation_store.relations[v.into_array_index()]
                            .children
                            .iter()
                            .map(|&x| (mt.clone(), x)),
                    );
                }
            }
        }
    }
}

pub struct ViewIdentifierAllocator {
    last_free_identifier: NonZeroUsize,
    free_identifier: BTreeSet<ViewIdentifier>,
    last_free_group_identifier: NonZeroUsize,
    free_group_identifier: BTreeSet<ViewGroupID>,
}
impl ViewIdentifierAllocator {
    pub fn new() -> Self {
        Self {
            last_free_identifier: unsafe { NonZeroUsize::new_unchecked(1) },
            free_identifier: BTreeSet::new(),
            last_free_group_identifier: unsafe { NonZeroUsize::new_unchecked(1) },
            free_group_identifier: BTreeSet::new(),
        }
    }
}

struct ViewInstanceCell {
    instance: Option<Box<dyn View>>,
    active: bool,
}

pub struct ViewInstanceStore {
    instances: Vec<ViewInstanceCell>,
}
impl ViewInstanceStore {
    pub fn new() -> Self {
        Self {
            instances: Vec::new(),
        }
    }
}

pub struct ViewEventHandlerStore {
    event_handler: Vec<Weak<dyn ViewEventHandler>>,
}
impl ViewEventHandlerStore {
    pub fn new() -> Self {
        Self {
            event_handler: Vec::new(),
        }
    }
}

struct ViewTreeRelation {
    parent: Option<ViewIdentifier>,
    children: Vec<ViewIdentifier>,
}
pub struct ViewTreeRelationStore {
    relations: Vec<ViewTreeRelation>,
}
impl ViewTreeRelationStore {
    pub fn new() -> Self {
        Self {
            relations: Vec::new(),
        }
    }
}

struct ViewRenderState {
    current_mounted_on: Option<(RawMountTarget, KeyboardFocusGroupRef)>,
    active_render_element_ct: Option<CompositeTreeRef>,
    active_render_element_ht: Option<HitTestTreeRef>,
    active_keyboard_focus_token: Option<FocusTargetToken>,
    visible: Option<bool>,
}
impl ViewRenderState {
    const EMPTY: Self = Self {
        current_mounted_on: None,
        active_render_element_ct: None,
        active_render_element_ht: None,
        active_keyboard_focus_token: None,
        visible: None,
    };
}

pub struct ViewRenderStateStore(Vec<ViewRenderState>);
impl ViewRenderStateStore {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

pub struct ViewGroupRelationStore {
    joining_group: Vec<Option<ViewGroupID>>,
    participants: Vec<HashSet<ViewIdentifier>>,
}
impl ViewGroupRelationStore {
    pub fn new() -> Self {
        Self {
            joining_group: Vec::new(),
            participants: Vec::new(),
        }
    }
}

#[deprecated = "use render-teardown based view lifecycle"]
pub fn alloc_view_id_without_instance(
    allocator: &mut ViewIdentifierAllocator,
    instance_store: &mut ViewInstanceStore,
    event_handler_store: &mut ViewEventHandlerStore,
    tree_relation_store: &mut ViewTreeRelationStore,
    group_relation_store: &mut ViewGroupRelationStore,
    render_state_store: &mut ViewRenderStateStore,
) -> ViewIdentifier {
    if let Some(id) = allocator.free_identifier.pop_first() {
        // reuse
        instance_store.instances[id.into_array_index()] = ViewInstanceCell {
            instance: None,
            active: true,
        };
        event_handler_store.event_handler[id.into_array_index()] =
            Weak::<EmptyViewEventHandler>::new();
        tree_relation_store.relations[id.into_array_index()] = ViewTreeRelation {
            parent: None,
            children: Vec::new(),
        };
        group_relation_store.joining_group[id.into_array_index()] = None;
        render_state_store.0[id.into_array_index()] = ViewRenderState::EMPTY;

        return id;
    }

    let id = ViewIdentifier(allocator.last_free_identifier);
    allocator.last_free_identifier = allocator
        .last_free_identifier
        .checked_add(1)
        .expect("too many views!");
    instance_store.instances.push(ViewInstanceCell {
        instance: None,
        active: true,
    });
    event_handler_store
        .event_handler
        .push(Weak::<EmptyViewEventHandler>::new());
    tree_relation_store.relations.push(ViewTreeRelation {
        parent: None,
        children: Vec::new(),
    });
    group_relation_store.joining_group.push(None);
    render_state_store.0.push(ViewRenderState::EMPTY);
    id
}

pub fn construct_view(
    ctor: impl FnOnce(ViewIdentifier) -> Box<dyn View>,
    allocator: &mut ViewIdentifierAllocator,
    instance_store: &mut ViewInstanceStore,
    event_handler_store: &mut ViewEventHandlerStore,
    tree_relation_store: &mut ViewTreeRelationStore,
    group_relation_store: &mut ViewGroupRelationStore,
    render_state_store: &mut ViewRenderStateStore,
) -> ViewIdentifier {
    if let Some(id) = allocator.free_identifier.pop_first() {
        // reuse
        instance_store.instances[id.into_array_index()] = ViewInstanceCell {
            instance: Some(ctor(id)),
            active: true,
        };
        event_handler_store.event_handler[id.into_array_index()] =
            Weak::<EmptyViewEventHandler>::new();
        tree_relation_store.relations[id.into_array_index()] = ViewTreeRelation {
            parent: None,
            children: Vec::new(),
        };
        group_relation_store.joining_group[id.into_array_index()] = None;
        render_state_store.0[id.into_array_index()] = ViewRenderState::EMPTY;

        return id;
    }

    let id = ViewIdentifier(allocator.last_free_identifier);
    allocator.last_free_identifier = allocator
        .last_free_identifier
        .checked_add(1)
        .expect("too many views!");
    instance_store.instances.push(ViewInstanceCell {
        instance: Some(ctor(id)),
        active: true,
    });
    event_handler_store
        .event_handler
        .push(Weak::<EmptyViewEventHandler>::new());
    tree_relation_store.relations.push(ViewTreeRelation {
        parent: None,
        children: Vec::new(),
    });
    group_relation_store.joining_group.push(None);
    render_state_store.0.push(ViewRenderState::EMPTY);
    id
}

pub fn free_view(
    id: ViewIdentifier,
    allocator: &mut ViewIdentifierAllocator,
    instance_store: &mut ViewInstanceStore,
    event_handler_store: &mut ViewEventHandlerStore,
    tree_relation_store: &mut ViewTreeRelationStore,
    group_relation_store: &mut ViewGroupRelationStore,
    render_state_store: &mut ViewRenderStateStore,
) {
    // ensure no parent/group owns this item
    view_detach_parent(id, tree_relation_store);
    leave_view_group(id, group_relation_store);

    if id.0.get() + 1 == allocator.last_free_identifier.get() {
        // returned last identifier
        allocator.last_free_identifier = id.0;
        instance_store.instances.pop();
        event_handler_store.event_handler.pop();
        tree_relation_store.relations.pop();
        group_relation_store.joining_group.pop();
        render_state_store.0.pop();

        return;
    }

    allocator.free_identifier.insert(id);
    // clear heap references
    instance_store.instances[id.into_array_index()].instance = None;
    event_handler_store.event_handler[id.into_array_index()] = Weak::<EmptyViewEventHandler>::new();
}

pub fn set_view_event_handler(
    id: ViewIdentifier,
    handler: &Rc<impl ViewEventHandler + 'static>,
    event_handler_store: &mut ViewEventHandlerStore,
) {
    event_handler_store.event_handler[id.into_array_index()] = Rc::downgrade(handler) as _;
}

pub fn call_view_update(
    target: ViewIdentifier,
    context: &mut ViewUpdateContext,
    event_handler_store: &mut ViewEventHandlerStore,
) {
    if let Some(h) = event_handler_store.event_handler[target.into_array_index()].upgrade() {
        h.update(context);
    }
}

pub fn view_set_parent(
    id: ViewIdentifier,
    parent: ViewIdentifier,
    tree_relation_store: &mut ViewTreeRelationStore,
) {
    if let Some(p) = tree_relation_store.relations[id.into_array_index()]
        .parent
        .replace(parent)
    {
        if p == parent {
            // same parent
            return;
        }

        // unlink from old parent
        tree_relation_store.relations[p.into_array_index()]
            .children
            .retain(|&x| x != id);
    }

    tree_relation_store.relations[parent.into_array_index()]
        .children
        .push(id);
}

pub fn view_detach_parent(id: ViewIdentifier, tree_relation_store: &mut ViewTreeRelationStore) {
    if let Some(p) = tree_relation_store.relations[id.into_array_index()]
        .parent
        .take()
    {
        tree_relation_store.relations[p.into_array_index()]
            .children
            .retain(|&x| x != id);
    }
}

pub fn alloc_view_group(
    allocator: &mut ViewIdentifierAllocator,
    group_relation_store: &mut ViewGroupRelationStore,
) -> ViewGroupID {
    if let Some(id) = allocator.free_group_identifier.pop_first() {
        return id;
    }

    let r = ViewGroupID(allocator.last_free_group_identifier);
    allocator.last_free_group_identifier = allocator
        .last_free_group_identifier
        .checked_add(1)
        .expect("too many view groups!");
    group_relation_store.participants.push(HashSet::new());
    r
}

pub fn free_view_group(
    id: ViewGroupID,
    allocator: &mut ViewIdentifierAllocator,
    group_relation_store: &mut ViewGroupRelationStore,
) {
    // ensure no view participants to this view
    for x in group_relation_store.participants[id.into_array_index()]
        .drain()
        .collect::<Vec<_>>()
    {
        group_relation_store.joining_group[x.into_array_index()] = None;
    }

    if id.0.get() + 1 == allocator.last_free_group_identifier.get() {
        // returned last identifier
        allocator.last_free_group_identifier = id.0;
        group_relation_store.participants.pop();

        return;
    }

    allocator.free_group_identifier.insert(id);
}

pub fn join_view_group(
    id: ViewIdentifier,
    group_id: ViewGroupID,
    group_relation_store: &mut ViewGroupRelationStore,
) {
    if let Some(g) = group_relation_store.joining_group[id.into_array_index()].replace(group_id) {
        if g == group_id {
            // same group
            return;
        }

        group_relation_store.participants[g.into_array_index()].remove(&id);
    }

    group_relation_store.participants[group_id.into_array_index()].insert(id);
}

pub fn leave_view_group(id: ViewIdentifier, group_relation_store: &mut ViewGroupRelationStore) {
    let Some(g) = group_relation_store.joining_group[id.into_array_index()].take() else {
        // not joining any group
        return;
    };

    group_relation_store.participants[g.into_array_index()].remove(&id);
}

pub fn view_iter_self_group_participants(
    id: ViewIdentifier,
    group_relation_store: &ViewGroupRelationStore,
) -> impl Iterator<Item = ViewIdentifier> + '_ {
    group_relation_store
        .joining_group
        .get(id.into_array_index())
        .and_then(|&x| group_relation_store.participants.get(x?.into_array_index()))
        .into_iter()
        .flat_map(|x| x.iter().copied())
}

pub fn render_view_recursive(
    target: ViewIdentifier,
    ctx: &mut RenderContext,
    mount_on: &(impl MountTarget + ?Sized),
    keyboard_focus_group: KeyboardFocusGroupRef,
    instance_store: &mut ViewInstanceStore,
    event_handler_store: &mut ViewEventHandlerStore,
    tree_relation_store: &ViewTreeRelationStore,
    render_state_store: &mut ViewRenderStateStore,
) {
    let mut scheduled_renders = VecDeque::new();
    scheduled_renders.push_back((RawMountTarget::from_typed(mount_on), target));
    while let Some((mt, v)) = scheduled_renders.pop_front() {
        let mut sched = RenderChildScheduler::new();
        render_view_instance1(
            v,
            ctx,
            &mut sched,
            mt,
            keyboard_focus_group,
            instance_store,
            event_handler_store,
            render_state_store,
        );

        if let Some(mt) = sched.mount_on {
            // schedule render children to mount on
            scheduled_renders.extend(
                tree_relation_store.relations[v.into_array_index()]
                    .children
                    .iter()
                    .map(|&x| (mt.clone(), x)),
            );
        }
    }
}

fn render_view_instance1(
    target: ViewIdentifier,
    ctx: &mut RenderContext,
    sched: &mut RenderChildScheduler,
    mount_to: RawMountTarget,
    kf_group: KeyboardFocusGroupRef,
    instance_store: &mut ViewInstanceStore,
    event_handler_store: &mut ViewEventHandlerStore,
    render_state_store: &mut ViewRenderStateStore,
) {
    let Some(&mut ViewInstanceCell {
        instance: Some(ref mut instance),
        active,
    }) = instance_store.instances.get_mut(target.into_array_index())
    else {
        // no instance associated
        return;
    };

    let new_render_elements = instance.render(
        &mut ViewInstanceModifier {
            event_handler_ref: &mut event_handler_store.event_handler[target.into_array_index()],
        },
        ctx,
        sched,
    );

    let render_state = &mut render_state_store.0[target.into_array_index()];
    // update render elements relations
    if render_state
        .current_mounted_on
        .as_ref()
        .map(|x| x.0.ct_root)
        != Some(mount_to.ct_root)
    {
        // parent changed
        let ct = new_render_elements
            .composite_tree
            .or(render_state.active_render_element_ct);
        if let Some(ct) = ct {
            ctx.composite_tree.add_child(mount_to.ct_root, ct);
            render_state.active_render_element_ct = Some(ct);
        }
    } else if let Some(new_ct) = new_render_elements.composite_tree
        && Some(new_ct) != render_state.active_render_element_ct
    {
        // different object rendered
        ctx.composite_tree.add_child(mount_to.ct_root, new_ct);
        render_state.active_render_element_ct = Some(new_ct);
    }

    if render_state
        .current_mounted_on
        .as_ref()
        .map(|x| x.0.ht_root)
        != Some(mount_to.ht_root)
    {
        // parent changed
        let ht = new_render_elements
            .hit_tree
            .or(render_state.active_render_element_ht);
        if let Some(ht) = ht {
            ctx.ht_manager.add_child(mount_to.ht_root, ht);
            render_state.active_render_element_ht = Some(ht);
        }
    } else if let Some(new_ht) = new_render_elements.hit_tree
        && Some(new_ht) != render_state.active_render_element_ht
    {
        // different object rendered
        ctx.ht_manager.add_child(mount_to.ht_root, new_ht);
        render_state.active_render_element_ht = Some(new_ht);
    }

    let new_active_keyboard_focus_token = new_render_elements
        .keyboard_focus
        .or(render_state.active_keyboard_focus_token);
    if Some(kf_group) != render_state.current_mounted_on.as_ref().map(|x| x.1) {
        // group changed
        if let Some(kf) = new_active_keyboard_focus_token {
            ctx.keyboard_focus_registry.join_group(kf_group, kf);
            render_state.active_keyboard_focus_token = Some(kf);
        }
    } else if let Some(kf) = new_active_keyboard_focus_token
        && Some(kf) != render_state.active_keyboard_focus_token
    {
        // different token issued
        ctx.keyboard_focus_registry.join_group(kf_group, kf);
        render_state.active_keyboard_focus_token = Some(kf);
    }

    render_state.current_mounted_on = Some((mount_to, kf_group));

    if render_state.visible.replace(active) != Some(active) {
        // visible state changed
        if let Some(ct) = render_state.active_render_element_ct {
            ctx.composite_tree
                .begin_mod_chain(ct)
                .set_active(active)
                .apply();
        }

        if let Some(ht) = render_state.active_render_element_ht {
            ctx.ht_manager.get_data_mut(ht).active = active;
        }
    }
}

pub fn teardown_view_recursive(
    target: ViewIdentifier,
    ctx: &mut TeardownContext,
    instance_store: &mut ViewInstanceStore,
    tree_relation_store: &ViewTreeRelationStore,
    render_state_store: &mut ViewRenderStateStore,
) {
    // 逆向きに(深いものから)teardownしていく
    let mut scheduled_teardowns = Vec::new();
    let mut descend_stack = VecDeque::new();
    descend_stack.push_back(target);
    while let Some(id) = descend_stack.pop_front() {
        scheduled_teardowns.push(id);
        descend_stack.extend(
            tree_relation_store
                .relations
                .get(id.into_array_index())
                .into_iter()
                .flat_map(|x| x.children.iter().copied()),
        );
    }

    for v in scheduled_teardowns {
        if let ViewInstanceCell {
            instance: Some(ref mut instance),
            ..
        } = instance_store.instances[v.into_array_index()]
        {
            instance.teardown(ctx);
        }
        render_state_store.0[v.into_array_index()] = ViewRenderState::EMPTY;
    }
}

pub fn view_instance<T: View + 'static>(
    id: ViewIdentifier,
    instance_store: &ViewInstanceStore,
) -> Option<&T> {
    (instance_store
        .instances
        .get(id.into_array_index())?
        .instance
        .as_ref()?
        .as_ref() as &dyn core::any::Any)
        .downcast_ref::<T>()
}

pub fn view_instance_mut<T: View + 'static>(
    id: ViewIdentifier,
    instance_store: &mut ViewInstanceStore,
) -> Option<&mut T> {
    (instance_store
        .instances
        .get_mut(id.into_array_index())?
        .instance
        .as_mut()?
        .as_mut() as &mut dyn core::any::Any)
        .downcast_mut::<T>()
}

pub fn view_set_visibility(
    id: ViewIdentifier,
    visible: bool,
    instance_store: &mut ViewInstanceStore,
) {
    let Some(instance) = instance_store.instances.get_mut(id.into_array_index()) else {
        return;
    };

    instance.active = visible;
}

pub trait ViewRegisterable {
    fn construct_view(
        &mut self,
        ctor: impl FnOnce(ViewIdentifier) -> Box<dyn View>,
    ) -> ViewIdentifier;
    fn free_view(&mut self, id: ViewIdentifier);
}

pub trait ViewGroupRegisterable {
    fn create_view_group(&mut self) -> ViewGroupID;
    fn destroy_view_group(&mut self, id: ViewGroupID);
}

pub trait ViewInstanceQueryable {
    fn view_instance<T: View + 'static>(&self, id: ViewIdentifier) -> Option<&T>;
}
pub trait ViewInstanceQueryableMut {
    fn view_instance_mut<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T>;
    fn view_set_visibility(&mut self, id: ViewIdentifier, visible: bool);
}

pub trait ViewRelationControllable {
    fn view_set_parent(&mut self, id: ViewIdentifier, parent: ViewIdentifier);
    fn view_detach_parent(&mut self, id: ViewIdentifier);
}

pub trait ViewImmediateRenderable {
    fn render_view_recursive(
        &mut self,
        target: ViewIdentifier,
        mount_on: &(impl MountTarget + ?Sized),
        keyboard_focus_group: KeyboardFocusGroupRef,
    );
}

pub trait ViewRenderer {
    fn schedule_view_render(&mut self, target: ViewIdentifier);
}

pub trait ViewImmediateTeardownable {
    fn teardown_view_recursive(&mut self, target: ViewIdentifier);
}

pub trait ViewEventHandler {
    #[allow(unused_variables)]
    fn update(&self, context: &mut ViewUpdateContext) {}
}

struct EmptyViewEventHandler;
impl ViewEventHandler for EmptyViewEventHandler {}

pub enum ViewFeedbackRegistryDelayedOps {
    SubscribePerformAtomic(Weak<dyn ViewFeedbackHandler<ViewFeedbackPerformAtomic>>),
    Subscribe(core::any::TypeId, ViewFeedbackHandlerUntyped),
    UnsubscribePerformAtomic(Weak<dyn ViewFeedbackHandler<ViewFeedbackPerformAtomic>>),
    Unsubscribe(core::any::TypeId, ViewFeedbackHandlerUntyped),
}
impl ViewFeedbackRegistryDelayedOps {
    fn subscribe<T: 'static>(handler: &Rc<impl ViewFeedbackHandler<T> + 'static>) -> Self {
        let tyid = core::any::TypeId::of::<T>();
        if tyid == core::any::TypeId::of::<ViewFeedbackPerformAtomic>() {
            // optimize: specific handler array for PerformAtomic feedbacks
            // TがViewFeedbackPerformAtomicとおなじなのは確認済みなのでゴリゴリ強制する
            Self::SubscribePerformAtomic(unsafe {
                Weak::from_raw(core::mem::transmute::<
                    _,
                    *const dyn ViewFeedbackHandler<ViewFeedbackPerformAtomic>,
                >(
                    (Rc::downgrade(handler) as Weak<dyn ViewFeedbackHandler<T>>).into_raw(),
                ))
            })
        } else {
            Self::Subscribe(
                tyid,
                ViewFeedbackHandlerUntyped::from_typed(Rc::downgrade(handler) as _),
            )
        }
    }

    fn unsubscribe<T: 'static>(handler: &Rc<impl ViewFeedbackHandler<T> + 'static>) -> Self {
        let tyid = core::any::TypeId::of::<T>();
        if tyid == core::any::TypeId::of::<ViewFeedbackPerformAtomic>() {
            // optimize: specific handler array for PerformAtomic feedbacks
            // TがViewFeedbackPerformAtomicとおなじなのは確認済みなのでゴリゴリ強制する
            Self::UnsubscribePerformAtomic(unsafe {
                Weak::from_raw(core::mem::transmute::<
                    _,
                    *const dyn ViewFeedbackHandler<ViewFeedbackPerformAtomic>,
                >(
                    (Rc::downgrade(handler) as Weak<dyn ViewFeedbackHandler<T>>).into_raw(),
                ))
            })
        } else {
            Self::Unsubscribe(
                tyid,
                ViewFeedbackHandlerUntyped::from_typed(Rc::downgrade(handler) as _),
            )
        }
    }
}

pub struct ViewFeedbackRegistry {
    perform_atomic_feedback_receivers:
        Vec<Weak<dyn ViewFeedbackHandler<ViewFeedbackPerformAtomic>>>,
    feedback_receivers: HashMap<core::any::TypeId, Vec<ViewFeedbackHandlerUntyped>>,
}
impl ViewFeedbackRegistry {
    pub fn new() -> Self {
        Self {
            perform_atomic_feedback_receivers: Vec::new(),
            feedback_receivers: HashMap::new(),
        }
    }

    pub fn perform_delayed(&mut self, ops: &mut VecDeque<ViewFeedbackRegistryDelayedOps>) {
        for op in ops.drain(..) {
            match op {
                ViewFeedbackRegistryDelayedOps::SubscribePerformAtomic(weak) => {
                    self.perform_atomic_feedback_receivers.push(weak);
                }
                ViewFeedbackRegistryDelayedOps::Subscribe(tyid, handler) => {
                    self.feedback_receivers
                        .entry(tyid)
                        .or_insert_with(Vec::new)
                        .push(handler);
                }
                ViewFeedbackRegistryDelayedOps::UnsubscribePerformAtomic(weak) => {
                    self.perform_atomic_feedback_receivers
                        .retain(|h| !h.ptr_eq(&weak));
                }
                ViewFeedbackRegistryDelayedOps::Unsubscribe(tyid, handler) => {
                    self.feedback_receivers
                        .entry(tyid)
                        .or_insert_with(Vec::new)
                        .retain(|h| !h.target.ptr_eq(&handler.target));
                }
            }
        }
    }

    pub fn perform_atomic<'a, 'h>(&self, context: &mut ViewFeedbackContext<'a, 'h>) {
        for x in &self.perform_atomic_feedback_receivers {
            let Some(x) = x.upgrade() else {
                continue;
            };

            x.accept_feedback(&ViewFeedbackPerformAtomic, context);
        }
    }

    pub fn dispatch<'a, 'h, T: 'static>(
        &self,
        feedback: T,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        let Some(subscribers) = self.feedback_receivers.get(&core::any::TypeId::of::<T>()) else {
            // no subscribers
            return;
        };

        for x in subscribers {
            unsafe {
                x.try_invoke(&feedback, context);
            }
        }
    }
}

pub struct ViewFeedbackContext<'a, 'h> {
    pub application: &'a Application,
    pub view_init_context: ViewInitContext<'a, 'h>,
    pub view_render_queue: &'a mut ViewRenderQueue,
}
impl ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    pub fn view_instance<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&T> {
        view_instance(id, self.view_init_context.view_instance_store)
    }

    #[inline(always)]
    pub fn view_instance_mut<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T> {
        view_instance_mut(id, self.view_init_context.view_instance_store)
    }

    #[inline(always)]
    pub fn view_set_parent(&mut self, id: ViewIdentifier, parent: ViewIdentifier) {
        view_set_parent(id, parent, self.view_init_context.view_tree_relation_store);
    }

    #[inline(always)]
    pub fn view_detach_parent(&mut self, id: ViewIdentifier) {
        view_detach_parent(id, self.view_init_context.view_tree_relation_store);
    }

    #[inline(always)]
    pub fn teardown_view_recursive(&mut self, target: ViewIdentifier) {
        teardown_view_recursive(
            target,
            &mut TeardownContext {
                mount_context: MountContext {
                    composite_tree: self.view_init_context.mount_context.composite_tree,
                    ht_manager: self.view_init_context.mount_context.ht_manager,
                    keyboard_focus_registry: self
                        .view_init_context
                        .mount_context
                        .keyboard_focus_registry,
                    current_sec: self.view_init_context.mount_context.current_sec,
                },
                view_feedback_subscription_delayed_ops: self
                    .view_init_context
                    .view_feedback_subscription_delayed_ops,
            },
            self.view_init_context.view_instance_store,
            self.view_init_context.view_tree_relation_store,
            self.view_init_context.view_render_state_store,
        );
    }

    #[inline(always)]
    pub fn schedule_render(&mut self, view: ViewIdentifier) {
        self.view_render_queue.schedule(view);
    }
}

pub trait ViewFeedbackHandler<T> {
    fn accept_feedback<'a, 'h>(&self, feedback: &T, context: &mut ViewFeedbackContext<'a, 'h>);
}

#[derive(Debug, Clone, Copy)]
pub struct ViewFeedbackPerformAtomic;

struct ViewFeedbackHandlerUntyped {
    target: Weak<dyn core::any::Any>,
    accept_feedback_fn:
        fn(this: *const (), feedback: *const (), context: &mut ViewFeedbackContext<'_, '_>),
}
impl ViewFeedbackHandlerUntyped {
    fn from_typed<T, E: ViewFeedbackHandler<T> + 'static>(target: Weak<E>) -> Self {
        Self {
            target,
            accept_feedback_fn: unsafe { core::mem::transmute(E::accept_feedback as *const ()) },
        }
    }

    unsafe fn try_invoke<'a, 'h, T>(
        &self,
        feedback: &T,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) -> bool {
        let Some(target) = self.target.upgrade() else {
            return false;
        };

        (self.accept_feedback_fn)(
            core::ptr::from_ref(target.as_ref()).cast(),
            core::ptr::from_ref(feedback).cast(),
            context,
        );

        true
    }
}

mod popup;
pub use self::popup::{
    OverlayPopupBasicFrameView, OverlayPopupBasicMaskView, Popup, PopupID, PopupManager,
};
mod dialog;
pub use self::dialog::*;

mod label;
pub use self::label::*;

mod button;
pub use self::button::{
    SimpleButtonConstantEventHandler, SimpleButtonEventHandler, SimpleButtonView,
};

mod menu;
pub use self::menu::{
    BaseSurfaceEventHandler as MenuBaseSurfaceEventHandler, CommandView as MenuItemCommandView,
    CommonResources as MenuItemCommonResources,
    DELAYED_ACTION_TIMEOUT_MS as MENU_DELAYED_ACTION_TIMEOUT_MS,
    HeadingView as MenuItemHeadingView, MenuItem, MenuItemLayout, MenuItemView,
    SeparatorView as MenuItemSeparatorView, SubMenuView as MenuItemSubMenuView,
};

mod text_input;
pub use self::text_input::*;

mod scroll;
pub use self::scroll::ScrollContainer;

pub mod dropdown_box;

pub mod checkbox;
pub use self::checkbox::{CheckboxView, ToggleButtonView};

mod radio;
pub use self::radio::*;
