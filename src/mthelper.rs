//! Multithread Support Helper

#[cfg(not(feature = "mt"))]
#[repr(transparent)]
pub struct DynamicMut<T>(std::cell::RefCell<T>);
#[cfg(feature = "mt")]
#[repr(transparent)]
pub struct DynamicMut<T>(parking_lot::RwLock<T>);
impl<T> From<T> for DynamicMut<T> {
    fn from(v: T) -> Self {
        Self::new(v)
    }
}
impl<T> DynamicMut<T> {
    pub fn new(v: T) -> Self {
        #[cfg(not(feature = "mt"))]
        {
            Self(std::cell::RefCell::new(v))
        }
        #[cfg(feature = "mt")]
        {
            Self(parking_lot::RwLock::new(v))
        }
    }

    #[cfg(not(feature = "mt"))]
    pub fn borrow(&self) -> std::cell::Ref<T> {
        self.0.borrow()
    }
    #[cfg(feature = "mt")]
    pub fn borrow(&self) -> parking_lot::RwLockReadGuard<T> {
        self.0.read()
    }

    #[cfg(not(feature = "mt"))]
    pub fn borrow_mut(&self) -> std::cell::RefMut<T> {
        self.0.borrow_mut()
    }
    #[cfg(feature = "mt")]
    pub fn borrow_mut(&self) -> parking_lot::RwLockWriteGuard<T> {
        self.0.write()
    }
}

#[cfg(not(feature = "mt"))]
pub use std::rc::Rc as SharedRef;
#[cfg(feature = "mt")]
pub use std::sync::Arc as SharedRef;
