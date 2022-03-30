//! Multithread Support Helper

pub trait DynamicMutabilityProvider<'a, T: 'a> {
    type BorrowType: Deref<Target = T> + 'a;
    type BorrowMutType: Deref<Target = T> + DerefMut + 'a;

    fn new(v: T) -> Self;
    fn borrow(&'a self) -> Self::BorrowType;
    fn borrow_mut(&'a self) -> Self::BorrowMutType;
    fn get_mut(&mut self) -> &mut T;
}
#[cfg(not(feature = "mt"))]
impl<'a, T: 'a> DynamicMutabilityProvider<'a, T> for std::cell::RefCell<T> {
    type BorrowType = std::cell::Ref<'a, T>;
    type BorrowMutType = std::cell::RefMut<'a, T>;

    fn new(v: T) -> Self {
        std::cell::RefCell::new(v)
    }
    fn borrow(&'a self) -> Self::BorrowType {
        std::cell::RefCell::borrow(self)
    }
    fn borrow_mut(&'a self) -> Self::BorrowMutType {
        std::cell::RefCell::borrow_mut(self)
    }
    fn get_mut(&mut self) -> &mut T {
        std::cell::RefCell::get_mut(self)
    }
}
#[cfg(feature = "mt")]
impl<'a, T: 'a> DynamicMutabilityProvider<'a, T> for parking_lot::RwLock<T> {
    type BorrowType = parking_lot::RwLockReadGuard<'a, T>;
    type BorrowMutType = parking_lot::RwLockWriteGuard<'a, T>;

    fn new(v: T) -> Self {
        parking_lot::RwLock::new(v)
    }
    fn borrow(&'a self) -> Self::BorrowType {
        parking_lot::RwLock::read(self)
    }
    fn borrow_mut(&'a self) -> Self::BorrowMutType {
        parking_lot::RwLock::write(self)
    }
    fn get_mut(&mut self) -> &mut T {
        parking_lot::RwLock::get_mut(self)
    }
}

pub trait MappableGuardObject<T, U> {
    type Output;

    fn map_guarded_value(self, trans: impl for<'v> FnOnce(&'v T) -> &'v U) -> Self::Output;
}
pub trait MappableMutGuardObject<T, U> {
    type Output;

    fn map_guarded_value(self, trans: impl for<'v> FnOnce(&'v mut T) -> &'v mut U) -> Self::Output;
}
#[cfg(not(feature = "mt"))]
impl<'x, T, U: 'x> MappableGuardObject<T, U> for std::cell::Ref<'x, T> {
    type Output = std::cell::Ref<'x, U>;

    fn map_guarded_value(self, trans: impl for<'v> FnOnce(&'v T) -> &'v U) -> Self::Output {
        std::cell::Ref::map(self, trans)
    }
}
#[cfg(not(feature = "mt"))]
impl<'x, T, U: 'x> MappableMutGuardObject<T, U> for std::cell::RefMut<'x, T> {
    type Output = std::cell::RefMut<'x, U>;

    fn map_guarded_value(self, trans: impl for<'v> FnOnce(&'v mut T) -> &'v mut U) -> Self::Output {
        std::cell::RefMut::map(self, trans)
    }
}
#[cfg(feature = "mt")]
impl<'x, T, U: 'x> MappableGuardObject<T, U> for parking_lot::RwLockReadGuard<'x, T> {
    type Output = parking_lot::MappedRwLockReadGuard<'x, U>;

    fn map_guarded_value(self, trans: impl for<'v> FnOnce(&'v T) -> &'v U) -> Self::Output {
        parking_lot::RwLockReadGuard::map(self, trans)
    }
}
#[cfg(feature = "mt")]
impl<'x, T, U: 'x> MappableMutGuardObject<T, U> for parking_lot::RwLockWriteGuard<'x, T> {
    type Output = parking_lot::MappedRwLockWriteGuard<'x, U>;

    fn map_guarded_value(self, trans: impl for<'v> FnOnce(&'v mut T) -> &'v mut U) -> Self::Output {
        parking_lot::RwLockWriteGuard::map(self, trans)
    }
}

#[cfg(feature = "mt")]
pub use parking_lot::RwLock as DynamicMut;
use std::ops::{Deref, DerefMut};
#[cfg(feature = "mt")]
pub use std::sync::Arc as SharedRef;
#[cfg(not(feature = "mt"))]
pub use std::{cell::RefCell as DynamicMut, rc::Rc as SharedRef};
