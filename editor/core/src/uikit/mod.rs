//! Non-Application related common ui kits

use std::{
    collections::{BTreeSet, HashMap, VecDeque},
    num::NonZeroUsize,
    rc::{Rc, Weak},
};

use crate::{
    Application, SyncEvent, SystemLink,
    input::{
        KeyboardFocusTokenRegistry,
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
            view_registry: &mut self.view_registry,
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
    pub view_registry: &'a mut ViewRegistry,
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

pub enum ViewElementSize {
    Automatic,
    Fixed(Size<LogicalUnit>),
}

pub struct ViewPlacement {
    pub location: ViewLocation,
    pub size: ViewElementSize,
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

pub struct ViewRegistry {
    last_free_identifier: NonZeroUsize,
    free_identifier: BTreeSet<NonZeroUsize>,
    event_handlers: Vec<Weak<dyn ViewEventHandler>>,
}
impl ViewRegistry {
    pub fn new() -> Self {
        Self {
            last_free_identifier: unsafe { NonZeroUsize::new_unchecked(1) },
            free_identifier: BTreeSet::new(),
            event_handlers: Vec::new(),
        }
    }

    pub fn alloc(&mut self) -> ViewIdentifier {
        if let Some(id) = self.free_identifier.pop_first() {
            return ViewIdentifier(id);
        }

        let r = ViewIdentifier(self.last_free_identifier);
        self.last_free_identifier = self
            .last_free_identifier
            .checked_add(1)
            .expect("too many views!");
        self.event_handlers
            .push(Weak::<EmptyViewEventHandler>::new());
        r
    }

    pub fn free(&mut self, id: ViewIdentifier) {
        if id.0.get() + 1 == self.last_free_identifier.get() {
            // returned last identifier
            self.last_free_identifier =
                unsafe { NonZeroUsize::new_unchecked(self.last_free_identifier.get() - 1) };
            self.event_handlers.pop();
            return;
        }

        self.free_identifier.insert(id.0);
        self.event_handlers[id.0.get() - 1] = Weak::<EmptyViewEventHandler>::new();
    }

    pub fn set_event_handler(
        &mut self,
        id: ViewIdentifier,
        handler: &Rc<impl ViewEventHandler + 'static>,
    ) {
        self.event_handlers[id.0.get() - 1] = Rc::downgrade(handler) as _;
    }

    pub fn call_update(&self, id: ViewIdentifier, context: &mut ViewUpdateContext) {
        let Some(eh) = self.event_handlers[id.0.get() - 1].upgrade() else {
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
    MultilineTextInputView, NumericInputView, NumericInputViewBackingStore, RawTextInputView,
    RawTextInputViewCreateFlags, TextInputView,
};

mod scroll;
pub use self::scroll::ScrollContainer;

pub mod dropdown_box;

pub mod checkbox;
pub use self::checkbox::{CheckboxView, ToggleButtonView};
