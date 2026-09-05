//! View Feedback: Notification Bus from model to view.

use core::any::TypeId;
use std::{
    collections::{HashMap, VecDeque},
    rc::{Rc, Weak},
};

use crate::{
    SyncEvent, SystemLink,
    input::{KeyboardFocusTokenRegistry, hittest::HitTestTreeManager},
    rendering::{MainThreadTextureIDIssuer, composite::CompositeTree},
};

pub trait ViewFeedbackHandler<T> {
    fn accept_feedback<'a, 'h>(&self, feedback: &T, context: &mut ViewFeedbackContext<'a, 'h>);
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

pub enum ViewFeedbackRegistryDelayedOps {
    SubscribePerformAtomic(Weak<dyn ViewFeedbackHandler<ViewFeedbackPerformAtomic>>),
    Subscribe(
        TypeId,
        #[allow(private_interfaces)] ViewFeedbackHandlerUntyped,
    ),
    UnsubscribePerformAtomic(Weak<dyn ViewFeedbackHandler<ViewFeedbackPerformAtomic>>),
    Unsubscribe(
        TypeId,
        #[allow(private_interfaces)] ViewFeedbackHandlerUntyped,
    ),
}
impl ViewFeedbackRegistryDelayedOps {
    pub fn make_subscribe<T: 'static>(handler: &Rc<impl ViewFeedbackHandler<T> + 'static>) -> Self {
        let tyid = TypeId::of::<T>();
        if tyid == TypeId::of::<ViewFeedbackPerformAtomic>() {
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

    pub fn make_unsubscribe<T: 'static>(
        handler: &Rc<impl ViewFeedbackHandler<T> + 'static>,
    ) -> Self {
        let tyid = TypeId::of::<T>();
        if tyid == TypeId::of::<ViewFeedbackPerformAtomic>() {
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

    pub unsafe fn dispatch_dynamic_unchecked<'a, 'h>(
        &self,
        feedback: *const (),
        feedback_type: &core::any::TypeId,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        let Some(subscribers) = self.feedback_receivers.get(feedback_type) else {
            // no subscribers
            return;
        };

        for x in subscribers {
            unsafe {
                x.try_invoke_untyped(feedback, context);
            }
        }
    }
}

pub struct ViewFeedbackContext<'a, 'h> {
    pub application: &'a model::Application,
    pub composite_tree: &'a mut CompositeTree<SyncEvent>,
    pub ht_manager: &'a mut HitTestTreeManager<'h>,
    pub keyboard_focus_registry: &'a mut KeyboardFocusTokenRegistry,
    pub current_sec: f32,
    pub view_allocator: &'a mut super::ViewIdentifierAllocator,
    pub view_instance_store: &'a mut super::ViewInstanceStore,
    pub view_tree_relation_store: &'a mut super::ViewTreeRelationStore,
    pub view_group_relation_store: &'a mut super::ViewGroupRelationStore,
    pub view_layout_state_store: &'a mut super::ViewLayoutStateStore,
    pub view_render_state_store: &'a mut super::ViewRenderStateStore,
    pub view_feedback_subscription_delayed_ops: &'a mut VecDeque<ViewFeedbackRegistryDelayedOps>,
    pub system_link: &'a SystemLink<'a>,
    pub main_thread_texture_id_issuer: &'a mut MainThreadTextureIDIssuer,
    pub view_render_queue: &'a mut super::ViewRenderQueue,
}
impl model::ApplicationAccess for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn application(&self) -> &model::Application {
        self.application
    }
}
impl super::ViewRegisterable for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn construct_view_direct<T: super::View + 'static>(
        &mut self,
        ctor: impl FnOnce(super::TypedViewIdentifier<T>) -> Box<T>,
    ) -> super::TypedViewIdentifier<T> {
        super::construct_view(
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
    fn free_view_untyped(&mut self, id: super::ViewIdentifier) {
        super::free_view(
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
impl super::ViewInstanceQueryable for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn view_instance_of<T: super::View + 'static>(&self, id: super::ViewIdentifier) -> Option<&T> {
        super::view_instance(id, self.view_instance_store)
    }
}
impl super::ViewInstanceQueryableMut for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn view_instance_mut_of<T: super::View + 'static>(
        &mut self,
        id: super::ViewIdentifier,
    ) -> Option<&mut T> {
        super::view_instance_mut(id, self.view_instance_store)
    }

    #[inline(always)]
    fn view_set_visibility_untyped(&mut self, id: super::ViewIdentifier, visible: bool) {
        super::view_set_visibility(id, visible, self.view_instance_store);
    }

    #[inline(always)]
    fn view_layout_mut_untyped(
        &mut self,
        id: super::ViewIdentifier,
    ) -> Option<&mut super::ViewLayout> {
        super::view_layout_mut(id, self.view_instance_store)
    }
}
impl super::ViewRelationControllable for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn view_set_parent_untyped(
        &mut self,
        id: super::ViewIdentifier,
        parent: super::ViewIdentifier,
    ) {
        super::view_set_parent(id, parent, self.view_tree_relation_store);
    }

    #[inline(always)]
    fn view_detach_parent_untyped(&mut self, id: super::ViewIdentifier) {
        super::view_detach_parent(id, self.view_tree_relation_store);
    }
}
impl super::ViewImmediateTeardownable for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn teardown_view_recursive_untyped(&mut self, target: super::ViewIdentifier) {
        super::teardown_view_recursive(
            target,
            &mut super::TeardownContext {
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
impl super::ViewRenderer for ViewFeedbackContext<'_, '_> {
    #[inline(always)]
    fn schedule_view_render_untyped(&mut self, view: super::ViewIdentifier) {
        self.view_render_queue.schedule(view);
    }
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

    unsafe fn try_invoke_untyped<'a, 'h>(
        &self,
        feedback: *const (),
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) -> bool {
        let Some(target) = self.target.upgrade() else {
            return false;
        };

        (self.accept_feedback_fn)(
            core::ptr::from_ref(target.as_ref()).cast(),
            feedback,
            context,
        );

        true
    }
}
