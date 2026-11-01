use core::{cell::UnsafeCell, sync::atomic::AtomicBool};

/// Simple spin-lock based mutex
pub struct Spinlocked<T> {
    value: UnsafeCell<T>,
    lock: AtomicBool,
}
unsafe impl<T> Sync for Spinlocked<T> {}
unsafe impl<T> Send for Spinlocked<T> {}
impl<T> Spinlocked<T> {
    #[inline(always)]
    pub const fn new(value: T) -> Self {
        Self {
            value: UnsafeCell::new(value),
            lock: AtomicBool::new(false),
        }
    }

    #[inline(always)]
    pub const fn get_mut(&mut self) -> &mut T {
        self.value.get_mut()
    }

    #[inline(always)]
    pub fn lock<'a>(&'a self) -> SpinlockedGuard<'a, T> {
        self.acquire_lock();
        SpinlockedGuard { lock: self }
    }

    #[inline(always)]
    fn acquire_lock(&self) {
        while self
            .lock
            .compare_exchange(
                false,
                true,
                core::sync::atomic::Ordering::Relaxed,
                core::sync::atomic::Ordering::Relaxed,
            )
            .is_err()
        {
            core::hint::spin_loop();
        }
    }

    #[inline(always)]
    fn release_lock(&self) {
        self.lock
            .store(false, core::sync::atomic::Ordering::Relaxed);
    }
}

#[repr(transparent)]
pub struct SpinlockedGuard<'a, T> {
    lock: &'a Spinlocked<T>,
}
impl<'a, T> Drop for SpinlockedGuard<'a, T> {
    #[inline(always)]
    fn drop(&mut self) {
        self.lock.release_lock();
    }
}
impl<'a, T> core::ops::Deref for SpinlockedGuard<'a, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.lock.value.get() }
    }
}
impl<'a, T> core::ops::DerefMut for SpinlockedGuard<'a, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.lock.value.get() }
    }
}
