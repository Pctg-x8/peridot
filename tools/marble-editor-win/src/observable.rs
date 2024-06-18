use std::{hash::Hash, rc::Rc};

use crate::{uikit::ViewContext, SharedMut};

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
