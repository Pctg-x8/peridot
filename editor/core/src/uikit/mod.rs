//! Non-Application related common ui kits

use std::{
    collections::{BTreeSet, HashMap, VecDeque},
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

pub struct MountContext<'a, 'h> {
    pub composite_tree: &'a mut CompositeTree<SyncEvent>,
    pub ht_manager: &'a mut HitTestTreeManager<'h>,
    pub keyboard_focus_registry: &'a mut KeyboardFocusTokenRegistry,
    pub current_sec: f32,
}

pub struct RenderContext<'env, 'h> {
    pub composite_tree: &'env mut CompositeTree<SyncEvent>,
    pub ht_manager: &'env mut HitTestTreeManager<'h>,
    pub keyboard_focus_registry: &'env mut KeyboardFocusTokenRegistry,
    pub current_sec: f32,
    pub system_link: &'env SystemLink<'env>,
    pub main_thread_texture_id_issuer: &'env mut MainThreadTextureIDIssuer,
    pub application: &'env Application,
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
    pub view_registry: &'a mut ViewRegistry,
    pub view_feedback_subscription_delayed_ops: &'a mut VecDeque<ViewFeedbackRegistryDelayedOps>,
    pub system_link: &'a SystemLink<'a>,
    pub ui_scale_factor: f32,
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
impl<'a, 'h> ViewInitContext<'a, 'h> {
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
        }
    }

    pub const fn make_render_context2<'env>(
        &'env mut self,
    ) -> (&'env mut ViewRegistry, RenderContext<'env, 'h>) {
        (
            self.view_registry,
            RenderContext {
                composite_tree: &mut self.mount_context.composite_tree,
                ht_manager: &mut self.mount_context.ht_manager,
                keyboard_focus_registry: &mut self.mount_context.keyboard_focus_registry,
                current_sec: self.mount_context.current_sec,
                system_link: self.system_link,
                main_thread_texture_id_issuer: self.main_thread_texture_id_issuer,
                application: self.application,
            },
        )
    }

    pub const fn derive<'a2>(&'a2 mut self) -> ViewInitContext<'a2, 'h> {
        ViewInitContext {
            mount_context: MountContext {
                composite_tree: &mut self.mount_context.composite_tree,
                ht_manager: &mut self.mount_context.ht_manager,
                keyboard_focus_registry: &mut self.mount_context.keyboard_focus_registry,
                current_sec: self.mount_context.current_sec,
            },
            view_registry: &mut self.view_registry,
            view_feedback_subscription_delayed_ops: &mut self
                .view_feedback_subscription_delayed_ops,
            ui_scale_factor: self.ui_scale_factor,
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

pub struct ViewLocation {
    pub parent_anchor: [f32; 2],
    pub anchor: [f32; 2],
    pub offset: Point<LogicalUnit>,
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

pub enum ViewElementSize {
    Automatic,
    Fixed(Size<LogicalUnit>),
}

pub struct ViewPlacement {
    pub location: ViewLocation,
    pub size: ViewElementSize,
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
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ViewIdentifier(NonZeroUsize);
impl core::fmt::Debug for ViewIdentifier {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ViewID#{}", self.0)
    }
}
impl ViewIdentifier {
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

struct ViewRegistryData {
    instance: Option<Box<dyn View>>,
    event_handler: Weak<dyn ViewEventHandler>,
    parent: Option<ViewIdentifier>,
    children: Vec<ViewIdentifier>,
}

pub struct ViewRegistry {
    last_free_identifier: NonZeroUsize,
    free_identifier: BTreeSet<NonZeroUsize>,
    instances: Vec<ViewRegistryData>,
}
impl ViewRegistry {
    pub fn new() -> Self {
        Self {
            last_free_identifier: unsafe { NonZeroUsize::new_unchecked(1) },
            free_identifier: BTreeSet::new(),
            instances: Vec::new(),
        }
    }

    #[deprecated = "use View trait based lifecycle management"]
    pub fn alloc_id_only(&mut self) -> ViewIdentifier {
        if let Some(id) = self.free_identifier.pop_first() {
            return ViewIdentifier(id);
        }

        let r = ViewIdentifier(self.last_free_identifier);
        self.last_free_identifier = self
            .last_free_identifier
            .checked_add(1)
            .expect("too many views!");
        self.instances.push(ViewRegistryData {
            instance: None,
            event_handler: Weak::<EmptyViewEventHandler>::new(),
            parent: None,
            children: Vec::new(),
        });
        r
    }

    pub fn alloc(&mut self, instance: Box<impl View + 'static>) -> ViewIdentifier {
        if let Some(id) = self.free_identifier.pop_first() {
            return ViewIdentifier(id);
        }

        let r = ViewIdentifier(self.last_free_identifier);
        self.last_free_identifier = self
            .last_free_identifier
            .checked_add(1)
            .expect("too many views!");
        self.instances.push(ViewRegistryData {
            instance: Some(instance as _),
            event_handler: Weak::<EmptyViewEventHandler>::new(),
            parent: None,
            children: Vec::new(),
        });
        r
    }

    pub fn construct(
        &mut self,
        ctor: impl FnOnce(ViewIdentifier) -> Box<dyn View>,
    ) -> ViewIdentifier {
        if let Some(id) = self.free_identifier.pop_first() {
            return ViewIdentifier(id);
        }

        let r = ViewIdentifier(self.last_free_identifier);
        let instance = ctor(r);
        self.last_free_identifier = self
            .last_free_identifier
            .checked_add(1)
            .expect("too many views!");
        self.instances.push(ViewRegistryData {
            instance: Some(instance),
            event_handler: Weak::<EmptyViewEventHandler>::new(),
            parent: None,
            children: Vec::new(),
        });
        r
    }

    pub fn free(&mut self, id: ViewIdentifier) {
        // ensure no parent owns this item
        self.detach_parent(id);

        if id.0.get() + 1 == self.last_free_identifier.get() {
            // returned last identifier
            self.last_free_identifier =
                unsafe { NonZeroUsize::new_unchecked(self.last_free_identifier.get() - 1) };
            self.instances.pop();
            return;
        }

        self.free_identifier.insert(id.0);
        self.instances[id.into_array_index()].event_handler = Weak::<EmptyViewEventHandler>::new();
        self.instances[id.into_array_index()].instance = None;
    }

    pub fn set_parent(&mut self, id: ViewIdentifier, parent: ViewIdentifier) {
        if let Some(p) = self.instances[id.into_array_index()].parent.replace(parent) {
            if p == parent {
                // same parent
                return;
            }

            // unlink from old parent
            self.instances[p.into_array_index()]
                .children
                .retain(|&x| x != id);
        }

        self.instances[parent.into_array_index()].children.push(id);
    }

    pub fn detach_parent(&mut self, id: ViewIdentifier) {
        if let Some(p) = self.instances[id.into_array_index()].parent.take() {
            self.instances[p.into_array_index()]
                .children
                .retain(|&x| x != id);
        }
    }

    pub fn render_recursive(
        &mut self,
        id: ViewIdentifier,
        ctx: &mut RenderContext,
        mount_on: &(impl MountTarget + ?Sized),
        keyboard_focus_group: KeyboardFocusGroupRef,
    ) {
        let mut scheduled_renders = VecDeque::new();
        scheduled_renders.push_back((RawMountTarget::from_typed(mount_on), id));
        while let Some((mt, v)) = scheduled_renders.pop_front() {
            let Some(data) = self.instances.get_mut(v.into_array_index()) else {
                // no data set
                continue;
            };
            let Some(ref mut instance) = data.instance else {
                // no instance associated
                continue;
            };

            let mut sched = RenderChildScheduler::new();
            instance
                .render(
                    &mut ViewInstanceModifier {
                        event_handler_ref: &mut data.event_handler,
                    },
                    ctx,
                    &mut sched,
                )
                .mount_on(&mt, keyboard_focus_group, &mut ctx.make_mount_context());
            if let Some(mt) = sched.mount_on {
                scheduled_renders.extend(data.children.iter().map(|&x| (mt.clone(), x)));
            }
        }
    }

    pub fn teardown_recursive(&mut self, id: ViewIdentifier, ctx: &mut TeardownContext) {
        // 逆向きに(深いものから)teardownしていく
        let mut scheduled_teardowns = Vec::new();
        let mut descend_stack = VecDeque::new();
        descend_stack.push_back(id);
        while let Some(id) = descend_stack.pop_front() {
            scheduled_teardowns.push(id);
            descend_stack.extend(
                self.instances
                    .get(id.into_array_index())
                    .into_iter()
                    .flat_map(|x| x.children.iter().copied()),
            );
        }

        for v in scheduled_teardowns {
            if let Some(ref mut instance) = self.instances[v.into_array_index()].instance {
                instance.teardown(ctx);
            }
        }
    }

    pub fn instance<T: View + 'static>(&self, id: ViewIdentifier) -> Option<&T> {
        (self
            .instances
            .get(id.into_array_index())?
            .instance
            .as_ref()?
            .as_ref() as &dyn core::any::Any)
            .downcast_ref::<T>()
    }

    pub fn instance_mut<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T> {
        (self
            .instances
            .get_mut(id.into_array_index())?
            .instance
            .as_mut()?
            .as_mut() as &mut dyn core::any::Any)
            .downcast_mut::<T>()
    }

    pub fn set_event_handler(
        &mut self,
        id: ViewIdentifier,
        handler: &Rc<impl ViewEventHandler + 'static>,
    ) {
        self.instances[id.into_array_index()].event_handler = Rc::downgrade(handler) as _;
    }

    pub fn call_update(&self, id: ViewIdentifier, context: &mut ViewUpdateContext) {
        let Some(eh) = self.instances[id.into_array_index()]
            .event_handler
            .upgrade()
        else {
            return;
        };

        eh.update(context);
    }
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
pub use self::text_input::{
    MultilineTextInputView, NumericInputView, NumericInputViewBackingStore,
    RawTextInputViewCreateFlags, TextInputView, TextInputViewCore, TextInputViewIO,
};

mod scroll;
pub use self::scroll::ScrollContainer;

pub mod dropdown_box;

pub mod checkbox;
pub use self::checkbox::{CheckboxView, ToggleButtonView};
