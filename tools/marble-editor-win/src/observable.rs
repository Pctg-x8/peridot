use std::{collections::HashSet, hash::Hash, rc::Rc};

use crate::{new_shared_mut, uikit::ViewContext, SharedMut, WeakMut};

#[repr(transparent)]
#[derive(Clone)]
pub struct ValueChangedEventHandlerHashKey<T>(pub SharedMut<dyn FnMut(&dyn ViewContext, T)>);
impl<T> PartialEq for ValueChangedEventHandlerHashKey<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
impl<T> Eq for ValueChangedEventHandlerHashKey<T> {}
impl<T> Hash for ValueChangedEventHandlerHashKey<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
    }
}

pub trait ObservationDisconnector {
    fn disconnect(&self);
}

#[repr(transparent)]
pub struct EventBusSubscriberHashKey<T>(pub SharedMut<dyn FnMut(T)>);
impl<T> core::cmp::PartialEq for EventBusSubscriberHashKey<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
impl<T> core::cmp::Eq for EventBusSubscriberHashKey<T> {}
impl<T> core::hash::Hash for EventBusSubscriberHashKey<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state)
    }
}

pub struct EventBusSubscriptionCancellationToken<T: Copy> {
    source: WeakMut<EventBusInner<T>>,
    subscriber: WeakMut<dyn FnMut(T)>,
}
impl<T: Copy> EventBusSubscriptionCancellationToken<T> {
    pub fn cancel(&self) {
        let (Some(src), Some(h)) = (self.source.upgrade(), self.subscriber.upgrade()) else {
            return;
        };

        src.borrow_mut()
            .subscribers
            .remove(&EventBusSubscriberHashKey(h));
    }
}
impl<T: Copy> ObservationDisconnector for EventBusSubscriptionCancellationToken<T> {
    fn disconnect(&self) {
        self.cancel()
    }
}

struct EventBusInner<T: Copy> {
    subscribers: HashSet<EventBusSubscriberHashKey<T>>,
}

pub struct EventBus<T: Copy>(SharedMut<EventBusInner<T>>);
impl<T: Copy> EventBus<T> {
    #[inline]
    pub fn new() -> Self {
        Self(new_shared_mut(EventBusInner {
            subscribers: HashSet::new(),
        }))
    }

    pub fn subscribe(
        &self,
        handler: impl FnMut(T) + 'static,
    ) -> EventBusSubscriptionCancellationToken<T> {
        let key = EventBusSubscriberHashKey(new_shared_mut(handler));
        let wk = Rc::downgrade(&key.0);
        self.0.borrow_mut().subscribers.insert(key);

        EventBusSubscriptionCancellationToken {
            source: Rc::downgrade(&self.0),
            subscriber: wk,
        }
    }

    #[inline]
    pub fn notify(&self, value: T) {
        for s in &self.0.borrow().subscribers {
            (&mut *s.0.borrow_mut())(value);
        }
    }
}
