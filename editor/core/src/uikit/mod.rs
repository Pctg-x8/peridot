//! Non-Application related common ui kits

use std::{
    collections::{BTreeSet, HashMap, HashSet, VecDeque},
    hash::Hash,
    num::NonZeroUsize,
    rc::{Rc, Weak},
};

use crate::{
    SyncEvent, SystemLink,
    input::{
        FocusTargetToken, KeyboardFocusGroupRef, KeyboardFocusTokenRegistry,
        hittest::{HitTestTreeManager, HitTestTreeRef},
    },
    model::{Application, ApplicationAccess},
    rendering::{
        MainThreadTextureIDIssuer,
        composite::{CompositeTree, CompositeTreeRef},
    },
    utils::{LogicalUnit, Rect, Size},
};

pub trait SystemLinkAccess {
    fn system_link<'a>(&'a self) -> &'a SystemLink<'a>;
}

pub struct MountContext<'a, 'h> {
    pub composite_tree: &'a mut CompositeTree<SyncEvent>,
    pub ht_manager: &'a mut HitTestTreeManager<'h>,
    pub keyboard_focus_registry: &'a mut KeyboardFocusTokenRegistry,
    pub current_sec: f32,
}

pub struct MeasureContext<'env> {
    pub system_link: &'env SystemLink<'env>,
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
impl ApplicationAccess for RenderContext<'_, '_> {
    #[inline(always)]
    fn application(&self) -> &Application {
        self.application
    }
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

pub struct ViewInitContext<'a, 'h> {
    pub mount_context: MountContext<'a, 'h>,
    pub view_allocator: &'a mut ViewIdentifierAllocator,
    pub view_instance_store: &'a mut ViewInstanceStore,
    pub view_tree_relation_store: &'a mut ViewTreeRelationStore,
    pub view_group_relation_store: &'a mut ViewGroupRelationStore,
    pub view_layout_state_store: &'a mut ViewLayoutStateStore,
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
    fn construct_view<T: View + 'static>(
        &mut self,
        ctor: impl FnOnce(TypedViewIdentifier<T>) -> Box<T>,
    ) -> TypedViewIdentifier<T> {
        construct_view(
            ctor,
            self.view_allocator,
            self.view_instance_store,
            self.view_tree_relation_store,
            self.view_group_relation_store,
            self.view_layout_state_store,
            self.view_render_state_store,
        )
    }

    fn free_view_untyped(&mut self, id: ViewIdentifier) {
        free_view(
            id,
            self.view_allocator,
            self.view_instance_store,
            self.view_tree_relation_store,
            self.view_group_relation_store,
            self.view_layout_state_store,
            self.view_render_state_store,
        )
    }
}
impl ViewRelationControllable for ViewInitContext<'_, '_> {
    fn view_set_parent_untyped(&mut self, id: ViewIdentifier, parent: ViewIdentifier) {
        view_set_parent(id, parent, self.view_tree_relation_store)
    }

    fn view_detach_parent_untyped(&mut self, id: ViewIdentifier) {
        view_detach_parent(id, self.view_tree_relation_store);
    }
}
impl ViewInstanceQueryable for ViewInitContext<'_, '_> {
    #[inline(always)]
    fn view_instance_of<T: View + 'static>(&self, id: ViewIdentifier) -> Option<&T> {
        view_instance(id, self.view_instance_store)
    }
}
impl ViewInstanceQueryableMut for ViewInitContext<'_, '_> {
    #[inline(always)]
    fn view_instance_mut_of<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T> {
        view_instance_mut(id, self.view_instance_store)
    }

    #[inline(always)]
    fn view_set_visibility_untyped(&mut self, id: ViewIdentifier, visible: bool) {
        crate::uikit::view_set_visibility(id, visible, self.view_instance_store);
    }

    #[inline(always)]
    fn view_layout_mut_untyped(&mut self, id: ViewIdentifier) -> Option<&mut ViewLayout> {
        view_layout_mut(id, self.view_instance_store)
    }
}
impl ViewImmediateRenderable for ViewInitContext<'_, '_> {
    fn render_view_with_base(
        &mut self,
        id: ViewIdentifier,
        mount_on: &(impl MountTarget + ?Sized),
        keyboard_focus_group: KeyboardFocusGroupRef,
        layout_rect: Rect<LogicalUnit>,
    ) {
        render_view_with_base(
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
            layout_rect,
            self.view_instance_store,
            self.view_tree_relation_store,
            self.view_layout_state_store,
            self.view_render_state_store,
        )
    }
}
impl ViewFeedbackRegisterable for ViewInitContext<'_, '_> {
    fn subscribe_view_feedback<T: 'static>(
        &mut self,
        handler: &Rc<impl ViewFeedbackHandler<T> + 'static>,
    ) {
        self.view_feedback_subscription_delayed_ops
            .push_back(ViewFeedbackRegistryDelayedOps::subscribe(handler));
    }

    fn unsubscribe_view_feedback<T: 'static>(
        &mut self,
        handler: &Rc<impl ViewFeedbackHandler<T> + 'static>,
    ) {
        self.view_feedback_subscription_delayed_ops
            .push_back(ViewFeedbackRegistryDelayedOps::unsubscribe(handler));
    }
}
impl ViewGroupRegisterable for ViewInitContext<'_, '_> {
    #[inline(always)]
    fn create_view_group(&mut self) -> ViewGroupID {
        alloc_view_group(self.view_allocator, self.view_group_relation_store)
    }

    #[inline(always)]
    fn destroy_view_group(&mut self, id: ViewGroupID) {
        free_view_group(id, self.view_allocator, self.view_group_relation_store);
    }
}
impl ViewGroupRelationControllable for ViewInitContext<'_, '_> {
    #[inline(always)]
    fn join_view_group_untyped(&mut self, id: ViewIdentifier, group: ViewGroupID) {
        join_view_group(id, group, self.view_group_relation_store);
    }

    #[inline(always)]
    fn leave_view_group_untyped(&mut self, id: ViewIdentifier) {
        leave_view_group(id, self.view_group_relation_store);
    }
}
impl<'a, 'h> ViewInitContext<'a, 'h> {
    #[deprecated = "use render-teardown based view lifecycle"]
    pub fn alloc_view_id_without_instance(&mut self) -> ViewIdentifier {
        alloc_view_id_without_instance(
            self.view_allocator,
            self.view_instance_store,
            self.view_tree_relation_store,
            self.view_group_relation_store,
            self.view_layout_state_store,
            self.view_render_state_store,
        )
    }

    pub const fn make_teardown_context<'a2>(&'a2 mut self) -> TeardownContext<'a2, 'h> {
        TeardownContext {
            composite_tree: &mut self.mount_context.composite_tree,
            ht_manager: &mut self.mount_context.ht_manager,
            keyboard_focus_registry: &mut self.mount_context.keyboard_focus_registry,
            current_sec: self.mount_context.current_sec,
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
            view_group_relation_store: &mut self.view_group_relation_store,
            view_layout_state_store: &mut self.view_layout_state_store,
            view_render_state_store: &mut self.view_render_state_store,
            view_feedback_subscription_delayed_ops: &mut self
                .view_feedback_subscription_delayed_ops,
            system_link: self.system_link,
            main_thread_texture_id_issuer: self.main_thread_texture_id_issuer,
            application: self.application,
        }
    }
}

pub struct TeardownContext<'a, 'h> {
    pub composite_tree: &'a mut CompositeTree<SyncEvent>,
    pub ht_manager: &'a mut HitTestTreeManager<'h>,
    pub keyboard_focus_registry: &'a mut KeyboardFocusTokenRegistry,
    pub current_sec: f32,
    pub view_feedback_subscription_delayed_ops: &'a mut VecDeque<ViewFeedbackRegistryDelayedOps>,
}
impl ViewFeedbackRegisterable for TeardownContext<'_, '_> {
    fn subscribe_view_feedback<T: 'static>(
        &mut self,
        handler: &Rc<impl ViewFeedbackHandler<T> + 'static>,
    ) {
        self.view_feedback_subscription_delayed_ops
            .push_back(ViewFeedbackRegistryDelayedOps::subscribe(handler));
    }

    fn unsubscribe_view_feedback<T: 'static>(
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

/// Viewの描画要素
pub struct ViewRenderElements {
    /// CompositeTree
    pub composite_tree: Option<CompositeTreeRef>,
    /// HitTestTree
    pub hit_tree: Option<HitTestTreeRef>,
    /// 子のmount対象とするCompositeTreeRef 指定しなければ`composite_tree`と同一
    pub mount_target_ct_override: Option<CompositeTreeRef>,
    /// 子のmount対象とするHitTestTreeRef 指定しなければ`hit_tree`と同一
    pub mount_target_ht_override: Option<HitTestTreeRef>,
    /// キーボードフォーカス
    pub keyboard_focus: Option<FocusTargetToken>,
}
impl ViewRenderElements {
    pub const EMPTY: Self = Self {
        composite_tree: None,
        hit_tree: None,
        mount_target_ct_override: None,
        mount_target_ht_override: None,
        keyboard_focus: None,
    };
}

/// Viewのライフサイクル
pub trait View: core::any::Any {
    /// Render(初回マウント/更新)時に呼ばれる
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements;

    /// Teardown(アンマウント)時に呼ばれる
    fn teardown(&mut self, ctx: &mut TeardownContext);

    /// 自身の推奨サイズを計算する
    fn measure_preferred_content_size(&self, ctx: &mut MeasureContext) -> Size<LogicalUnit>;

    /// 新しいLayout Layer(基準点を0, 0にもどす)をつくるかどうか
    fn create_new_layout_layer(&self) -> bool {
        false
    }
}

/// なにもしないView(他のViewをいれるためだけに使う)
pub struct ContainerView;
impl View for ContainerView {
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
pub struct TypedViewIdentifier<T>(ViewIdentifier, core::marker::PhantomData<*mut T>);
impl<T> Clone for TypedViewIdentifier<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}
impl<T> Copy for TypedViewIdentifier<T> {}
impl<T> PartialEq for TypedViewIdentifier<T> {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T> Eq for TypedViewIdentifier<T> {}
impl<T> PartialOrd for TypedViewIdentifier<T> {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<T> Ord for TypedViewIdentifier<T> {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}
impl<T> Hash for TypedViewIdentifier<T> {
    #[inline(always)]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}
impl<T> PartialEq<ViewIdentifier> for TypedViewIdentifier<T> {
    #[inline(always)]
    fn eq(&self, other: &ViewIdentifier) -> bool {
        self.0 == *other
    }
}
impl<T> PartialEq<TypedViewIdentifier<T>> for ViewIdentifier {
    #[inline(always)]
    fn eq(&self, other: &TypedViewIdentifier<T>) -> bool {
        *self == other.0
    }
}
impl<T> TypedViewIdentifier<T> {
    pub const fn into_untyped(self) -> ViewIdentifier {
        self.0
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

    #[profiler::instrument("View.RenderQueue.Perform")]
    pub fn perform(
        &mut self,
        ctx: &mut RenderContext,
        instance_store: &mut ViewInstanceStore,
        tree_relation_store: &ViewTreeRelationStore,
        layout_state_store: &mut ViewLayoutStateStore,
        render_state_store: &mut ViewRenderStateStore,
    ) {
        while let Some(mut target) = self.pending.pop_first() {
            if instance_store
                .instances
                .get(target.into_array_index())
                .is_none_or(|x| x.instance.is_none())
            {
                // invalid(already freed) instance
                continue;
            }

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

            layout_view_partial_recursive(
                target,
                &mut MeasureContext {
                    system_link: ctx.system_link,
                },
                instance_store,
                tree_relation_store,
                layout_state_store,
                |_| {},
            );
            let mut scheduled_renders = VecDeque::new();
            scheduled_renders.push_back((mount_target, target));
            while let Some((mt, v)) = scheduled_renders.pop_front() {
                let new_mount_target = render_view_instance1(
                    v,
                    ctx,
                    &mt,
                    kf_group,
                    instance_store,
                    layout_state_store,
                    render_state_store,
                );
                let next_mount_target = new_mount_target.unwrap_or(mt);

                // もうrenderしたので次のループからはRenderしない
                self.pending.remove(&v);
                scheduled_renders.extend(
                    tree_relation_store.relations[v.into_array_index()]
                        .children
                        .iter()
                        .map(|&x| (next_mount_target.clone(), x)),
                );
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
    pub(self) layout: ViewLayout,
}
impl ViewInstanceCell {
    #[inline(always)]
    fn new(instance: Option<Box<dyn View>>) -> Self {
        Self {
            instance,
            active: true,
            layout: Default::default(),
        }
    }
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

    #[inline(always)]
    pub(self) fn get(&self, id: ViewIdentifier) -> &ViewInstanceCell {
        &self.instances[id.into_array_index()]
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
    tree_relation_store: &mut ViewTreeRelationStore,
    group_relation_store: &mut ViewGroupRelationStore,
    layout_state_store: &mut ViewLayoutStateStore,
    render_state_store: &mut ViewRenderStateStore,
) -> ViewIdentifier {
    if let Some(id) = allocator.free_identifier.pop_first() {
        // reuse
        instance_store.instances[id.into_array_index()] = ViewInstanceCell::new(None);
        tree_relation_store.relations[id.into_array_index()] = ViewTreeRelation {
            parent: None,
            children: Vec::new(),
        };
        group_relation_store.joining_group[id.into_array_index()] = None;
        layout_state_store.set_empty(id);
        render_state_store.0[id.into_array_index()] = ViewRenderState::EMPTY;

        return id;
    }

    let id = ViewIdentifier(allocator.last_free_identifier);
    allocator.last_free_identifier = allocator
        .last_free_identifier
        .checked_add(1)
        .expect("too many views!");
    instance_store.instances.push(ViewInstanceCell::new(None));
    tree_relation_store.relations.push(ViewTreeRelation {
        parent: None,
        children: Vec::new(),
    });
    group_relation_store.joining_group.push(None);
    layout_state_store.push_empty();
    render_state_store.0.push(ViewRenderState::EMPTY);
    id
}

pub fn construct_view<T: View + 'static>(
    ctor: impl FnOnce(TypedViewIdentifier<T>) -> Box<T>,
    allocator: &mut ViewIdentifierAllocator,
    instance_store: &mut ViewInstanceStore,
    tree_relation_store: &mut ViewTreeRelationStore,
    group_relation_store: &mut ViewGroupRelationStore,
    layout_state_store: &mut ViewLayoutStateStore,
    render_state_store: &mut ViewRenderStateStore,
) -> TypedViewIdentifier<T> {
    if let Some(id) = allocator.free_identifier.pop_first() {
        // reuse
        let id = TypedViewIdentifier(id, core::marker::PhantomData);
        instance_store.instances[id.0.into_array_index()] = ViewInstanceCell::new(Some(ctor(id)));
        tree_relation_store.relations[id.0.into_array_index()] = ViewTreeRelation {
            parent: None,
            children: Vec::new(),
        };
        group_relation_store.joining_group[id.0.into_array_index()] = None;
        layout_state_store.set_empty(id.0);
        render_state_store.0[id.0.into_array_index()] = ViewRenderState::EMPTY;

        return id;
    }

    let id = TypedViewIdentifier(
        ViewIdentifier(allocator.last_free_identifier),
        core::marker::PhantomData,
    );
    allocator.last_free_identifier = allocator
        .last_free_identifier
        .checked_add(1)
        .expect("too many views!");
    instance_store
        .instances
        .push(ViewInstanceCell::new(Some(ctor(id))));
    tree_relation_store.relations.push(ViewTreeRelation {
        parent: None,
        children: Vec::new(),
    });
    group_relation_store.joining_group.push(None);
    layout_state_store.push_empty();
    render_state_store.0.push(ViewRenderState::EMPTY);
    id
}

pub fn free_view(
    id: ViewIdentifier,
    allocator: &mut ViewIdentifierAllocator,
    instance_store: &mut ViewInstanceStore,
    tree_relation_store: &mut ViewTreeRelationStore,
    group_relation_store: &mut ViewGroupRelationStore,
    layout_state_store: &mut ViewLayoutStateStore,
    render_state_store: &mut ViewRenderStateStore,
) {
    // ensure no parent/group owns this item
    view_detach_parent(id, tree_relation_store);
    leave_view_group(id, group_relation_store);

    if id.0.get() + 1 == allocator.last_free_identifier.get() {
        // returned last identifier
        allocator.last_free_identifier = id.0;
        instance_store.instances.pop();
        tree_relation_store.relations.pop();
        group_relation_store.joining_group.pop();
        layout_state_store.pop();
        render_state_store.0.pop();

        return;
    }

    allocator.free_identifier.insert(id);
    // clear heap references
    instance_store.instances[id.into_array_index()].instance = None;
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

#[profiler::instrument("View.RenderWithBase")]
pub fn render_view_with_base(
    target: ViewIdentifier,
    ctx: &mut RenderContext,
    mount_on: &(impl MountTarget + ?Sized),
    keyboard_focus_group: KeyboardFocusGroupRef,
    layout_rect: Rect<LogicalUnit>,
    instance_store: &mut ViewInstanceStore,
    tree_relation_store: &ViewTreeRelationStore,
    layout_state_store: &mut ViewLayoutStateStore,
    render_state_store: &mut ViewRenderStateStore,
) {
    layout_view_recursive(
        target,
        &mut MeasureContext {
            system_link: ctx.system_link,
        },
        layout_rect,
        instance_store,
        tree_relation_store,
        layout_state_store,
        &mut |_| {},
    );

    let mut scheduled_renders = VecDeque::new();
    scheduled_renders.push_back((RawMountTarget::from_typed(mount_on), target));
    while let Some((mt, v)) = scheduled_renders.pop_front() {
        let new_mount_target = render_view_instance1(
            v,
            ctx,
            &mt,
            keyboard_focus_group,
            instance_store,
            layout_state_store,
            render_state_store,
        );
        let next_mount_target = new_mount_target.unwrap_or(mt);

        scheduled_renders.extend(
            tree_relation_store.relations[v.into_array_index()]
                .children
                .iter()
                .map(|&x| (next_mount_target.clone(), x)),
        );
    }
}

#[profiler::instrument("View.Instance.Render")]
fn render_view_instance1(
    target: ViewIdentifier,
    ctx: &mut RenderContext,
    mount_to: &RawMountTarget,
    kf_group: KeyboardFocusGroupRef,
    instance_store: &mut ViewInstanceStore,
    layout_state_store: &ViewLayoutStateStore,
    render_state_store: &mut ViewRenderStateStore,
) -> Option<RawMountTarget> {
    let Some(&mut ViewInstanceCell {
        instance: Some(ref mut instance),
        active,
        ..
    }) = instance_store.instances.get_mut(target.into_array_index())
    else {
        // no instance associated
        return None;
    };

    let render_elements = instance.render(
        layout_state_store.get(target).layout_rect.clone(),
        ctx,
        layout_state_store,
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
        let ct = render_elements
            .composite_tree
            .or(render_state.active_render_element_ct);
        if let Some(ct) = ct {
            ctx.composite_tree.add_child(mount_to.ct_root, ct);
        }
    } else if render_state.active_render_element_ct != render_elements.composite_tree {
        // different object rendered
        if let Some(old) = render_state.active_render_element_ct {
            ctx.composite_tree.remove_child(old);
        }
        if let Some(new) = render_elements.composite_tree {
            ctx.composite_tree.add_child(mount_to.ct_root, new);
        }
    }
    render_state.active_render_element_ct = render_elements.composite_tree;

    if render_state
        .current_mounted_on
        .as_ref()
        .map(|x| x.0.ht_root)
        != Some(mount_to.ht_root)
    {
        // parent changed
        let ht = render_elements
            .hit_tree
            .or(render_state.active_render_element_ht);
        if let Some(ht) = ht {
            ctx.ht_manager.add_child(mount_to.ht_root, ht);
        }
    } else if render_state.active_render_element_ht != render_elements.hit_tree {
        // different object rendered
        if let Some(old) = render_state.active_render_element_ht {
            ctx.ht_manager.remove_child(old);
        }
        if let Some(new) = render_elements.hit_tree {
            ctx.ht_manager.add_child(mount_to.ht_root, new);
        }
    }
    render_state.active_render_element_ht = render_elements.hit_tree;

    let new_active_keyboard_focus_token = render_elements
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

    render_state.current_mounted_on = Some((mount_to.clone(), kf_group));

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

    if let Some(ct) = render_elements
        .mount_target_ct_override
        .or(render_state.active_render_element_ct)
        && let Some(ht) = render_elements
            .mount_target_ht_override
            .or(render_state.active_render_element_ht)
    {
        // 両方あるときだけこのViewの子にRenderできる
        Some(RawMountTarget {
            ct_root: ct,
            ht_root: ht,
        })
    } else {
        None
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

/// Teardown + Free Instance
pub fn destruct_view_recursive(
    target: ViewIdentifier,
    ctx: &mut TeardownContext,
    allocator: &mut ViewIdentifierAllocator,
    instance_store: &mut ViewInstanceStore,
    tree_relation_store: &mut ViewTreeRelationStore,
    group_relation_store: &mut ViewGroupRelationStore,
    layout_state_store: &mut ViewLayoutStateStore,
    render_state_store: &mut ViewRenderStateStore,
) {
    // ensure no parent owns this item
    view_detach_parent(target, tree_relation_store);
    assert!(
        tree_relation_store.relations[target.into_array_index()]
            .parent
            .is_none()
    );

    // 逆向きに(深いものから)teardownしていく
    let mut process_order = Vec::new();
    let mut descend_stack = VecDeque::new();
    descend_stack.push_back(target);
    while let Some(id) = descend_stack.pop_front() {
        tree_relation_store.relations[id.into_array_index()].parent = None;
        process_order.push(id);
        descend_stack.extend(
            tree_relation_store
                .relations
                .get_mut(id.into_array_index())
                .into_iter()
                .flat_map(|x| x.children.drain(..)),
        );
    }

    for id in process_order {
        let mut instance = instance_store.instances[id.into_array_index()]
            .instance
            .take();
        if let Some(ref mut instance) = instance {
            instance.teardown(ctx);
        }
        render_state_store.0[id.into_array_index()] = ViewRenderState::EMPTY;

        // ensure no parent/group owns this item
        leave_view_group(id, group_relation_store);

        if id.0.get() + 1 == allocator.last_free_identifier.get() {
            // returned last identifier
            allocator.last_free_identifier = id.0;
            instance_store.instances.pop();
            tree_relation_store.relations.pop();
            group_relation_store.joining_group.pop();
            layout_state_store.pop();
            render_state_store.0.pop();
        } else {
            allocator.free_identifier.insert(id);
        }
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

pub fn view_layout_mut(
    id: ViewIdentifier,
    instance_store: &mut ViewInstanceStore,
) -> Option<&mut ViewLayout> {
    Some(
        &mut instance_store
            .instances
            .get_mut(id.into_array_index())?
            .layout,
    )
}

pub trait ViewRegisterable {
    fn construct_view<T: View + 'static>(
        &mut self,
        ctor: impl FnOnce(TypedViewIdentifier<T>) -> Box<T>,
    ) -> TypedViewIdentifier<T>;
    fn free_view_untyped(&mut self, id: ViewIdentifier);

    #[inline(always)]
    fn free_view<T>(&mut self, id: TypedViewIdentifier<T>) {
        self.free_view_untyped(id.into_untyped());
    }
}

pub trait ViewGroupRegisterable {
    fn create_view_group(&mut self) -> ViewGroupID;
    fn destroy_view_group(&mut self, id: ViewGroupID);
}

pub trait ViewGroupRelationControllable {
    fn join_view_group_untyped(&mut self, id: ViewIdentifier, group: ViewGroupID);
    fn leave_view_group_untyped(&mut self, id: ViewIdentifier);

    #[inline(always)]
    fn join_view_group<T: View + 'static>(
        &mut self,
        id: TypedViewIdentifier<T>,
        group: ViewGroupID,
    ) {
        self.join_view_group_untyped(id.into_untyped(), group);
    }

    #[inline(always)]
    fn leave_view_group<T: View + 'static>(&mut self, id: TypedViewIdentifier<T>) {
        self.leave_view_group_untyped(id.into_untyped());
    }
}

pub trait ViewInstanceQueryable {
    fn view_instance_of<T: View + 'static>(&self, id: ViewIdentifier) -> Option<&T>;

    #[inline(always)]
    fn view_instance<T: View + 'static>(&self, id: TypedViewIdentifier<T>) -> Option<&T> {
        self.view_instance_of::<T>(id.into_untyped())
    }
}
pub trait ViewInstanceQueryableMut {
    fn view_instance_mut_of<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T>;
    fn view_set_visibility_untyped(&mut self, id: ViewIdentifier, visible: bool);
    fn view_layout_mut_untyped(&mut self, id: ViewIdentifier) -> Option<&mut ViewLayout>;

    #[inline(always)]
    fn view_instance_mut<T: View + 'static>(
        &mut self,
        id: TypedViewIdentifier<T>,
    ) -> Option<&mut T> {
        self.view_instance_mut_of::<T>(id.into_untyped())
    }

    #[inline(always)]
    fn view_set_visibility<T>(&mut self, id: TypedViewIdentifier<T>, visible: bool) {
        self.view_set_visibility_untyped(id.into_untyped(), visible)
    }

    #[inline(always)]
    fn view_layout_mut<T>(&mut self, id: TypedViewIdentifier<T>) -> Option<&mut ViewLayout> {
        self.view_layout_mut_untyped(id.into_untyped())
    }
}

pub trait ViewRelationControllable {
    fn view_set_parent_untyped(&mut self, id: ViewIdentifier, parent: ViewIdentifier);
    fn view_detach_parent_untyped(&mut self, id: ViewIdentifier);

    #[inline(always)]
    fn view_set_parent<T, U>(
        &mut self,
        id: TypedViewIdentifier<T>,
        parent: TypedViewIdentifier<U>,
    ) {
        self.view_set_parent_untyped(id.into_untyped(), parent.into_untyped())
    }

    #[inline(always)]
    fn view_detach_parent<T>(&mut self, id: TypedViewIdentifier<T>) {
        self.view_detach_parent_untyped(id.into_untyped())
    }
}

pub trait ViewRenderer {
    fn schedule_view_render_untyped(&mut self, target: ViewIdentifier);

    #[inline(always)]
    fn schedule_view_render<T>(&mut self, target: TypedViewIdentifier<T>) {
        self.schedule_view_render_untyped(target.into_untyped());
    }
}

pub trait ViewImmediateRenderable {
    fn render_view_with_base(
        &mut self,
        target: ViewIdentifier,
        mount_on: &(impl MountTarget + ?Sized),
        keyboard_focus_group: KeyboardFocusGroupRef,
        layout_rect: Rect<LogicalUnit>,
    );
}

pub trait ViewImmediateTeardownable {
    fn teardown_view_recursive_untyped(&mut self, target: ViewIdentifier);

    #[inline(always)]
    fn teardown_view_recursive<T>(&mut self, target: TypedViewIdentifier<T>) {
        self.teardown_view_recursive_untyped(target.into_untyped());
    }
}

pub trait ViewDestructionContext {
    fn destruct_view_recursive_untyped(&mut self, target: ViewIdentifier);

    #[inline(always)]
    fn destruct_view_recursive<T>(&mut self, target: TypedViewIdentifier<T>) {
        self.destruct_view_recursive_untyped(target.into_untyped());
    }
}

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

pub trait ViewFeedbackRegisterable {
    fn subscribe_view_feedback<T: 'static>(
        &mut self,
        handler: &Rc<impl ViewFeedbackHandler<T> + 'static>,
    );
    fn unsubscribe_view_feedback<T: 'static>(
        &mut self,
        handler: &Rc<impl ViewFeedbackHandler<T> + 'static>,
    );
}

pub struct ViewFeedbackContext<'a, 'h> {
    pub application: &'a Application,
    pub composite_tree: &'a mut CompositeTree<SyncEvent>,
    pub ht_manager: &'a mut HitTestTreeManager<'h>,
    pub keyboard_focus_registry: &'a mut KeyboardFocusTokenRegistry,
    pub current_sec: f32,
    pub view_allocator: &'a mut ViewIdentifierAllocator,
    pub view_instance_store: &'a mut ViewInstanceStore,
    pub view_tree_relation_store: &'a mut ViewTreeRelationStore,
    pub view_group_relation_store: &'a mut ViewGroupRelationStore,
    pub view_layout_state_store: &'a mut ViewLayoutStateStore,
    pub view_render_state_store: &'a mut ViewRenderStateStore,
    pub view_feedback_subscription_delayed_ops: &'a mut VecDeque<ViewFeedbackRegistryDelayedOps>,
    pub system_link: &'a SystemLink<'a>,
    pub main_thread_texture_id_issuer: &'a mut MainThreadTextureIDIssuer,
    pub view_render_queue: &'a mut ViewRenderQueue,
}
impl ApplicationAccess for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn application(&self) -> &Application {
        self.application
    }
}
impl ViewRegisterable for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn construct_view<T: View + 'static>(
        &mut self,
        ctor: impl FnOnce(TypedViewIdentifier<T>) -> Box<T>,
    ) -> TypedViewIdentifier<T> {
        construct_view(
            ctor,
            self.view_allocator,
            self.view_instance_store,
            self.view_tree_relation_store,
            self.view_group_relation_store,
            self.view_layout_state_store,
            self.view_render_state_store,
        )
    }

    #[inline(always)]
    fn free_view_untyped(&mut self, id: ViewIdentifier) {
        free_view(
            id,
            self.view_allocator,
            self.view_instance_store,
            self.view_tree_relation_store,
            self.view_group_relation_store,
            self.view_layout_state_store,
            self.view_render_state_store,
        )
    }
}
impl ViewInstanceQueryable for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn view_instance_of<T: View + 'static>(&self, id: ViewIdentifier) -> Option<&T> {
        view_instance(id, self.view_instance_store)
    }
}
impl ViewInstanceQueryableMut for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn view_instance_mut_of<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T> {
        view_instance_mut(id, self.view_instance_store)
    }

    #[inline(always)]
    fn view_set_visibility_untyped(&mut self, id: ViewIdentifier, visible: bool) {
        view_set_visibility(id, visible, self.view_instance_store);
    }

    #[inline(always)]
    fn view_layout_mut_untyped(&mut self, id: ViewIdentifier) -> Option<&mut ViewLayout> {
        view_layout_mut(id, self.view_instance_store)
    }
}
impl ViewRelationControllable for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn view_set_parent_untyped(&mut self, id: ViewIdentifier, parent: ViewIdentifier) {
        view_set_parent(id, parent, self.view_tree_relation_store);
    }

    #[inline(always)]
    fn view_detach_parent_untyped(&mut self, id: ViewIdentifier) {
        view_detach_parent(id, self.view_tree_relation_store);
    }
}
impl ViewImmediateTeardownable for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn teardown_view_recursive_untyped(&mut self, target: ViewIdentifier) {
        teardown_view_recursive(
            target,
            &mut TeardownContext {
                composite_tree: self.composite_tree,
                ht_manager: self.ht_manager,
                keyboard_focus_registry: self.keyboard_focus_registry,
                current_sec: self.current_sec,
                view_feedback_subscription_delayed_ops: self.view_feedback_subscription_delayed_ops,
            },
            self.view_instance_store,
            self.view_tree_relation_store,
            self.view_render_state_store,
        );
    }
}
impl ViewRenderer for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn schedule_view_render_untyped(&mut self, view: ViewIdentifier) {
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
    CommandView as MenuItemCommandView, CommonResources as MenuItemCommonResources,
    DELAYED_ACTION_TIMEOUT_MS as MENU_DELAYED_ACTION_TIMEOUT_MS, EventHandler as MenuEventHandler,
    MenuItem, MenuItemInteractableElement, MenuItemLayout, SubMenuView as MenuItemSubMenuView,
};

mod layout;
pub use self::layout::*;

mod text_input;
pub use self::text_input::*;

mod scroll;
pub use self::scroll::ScrollContainer;

pub mod dropdown_box;

pub mod checkbox;
pub use self::checkbox::{CheckboxView, ToggleButtonView};

mod radio;
pub use self::radio::*;
