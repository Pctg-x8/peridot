//! State Tracked Objects

use bedrock as br;

/// State-tracked Fence
pub enum StateFence<Fence: br::Fence> {
    Signaled(Fence),
    Unsignaled(Fence),
}
impl<Device: br::Device> StateFence<br::FenceObject<Device>> {
    /// Create a fence with Unsignaled state
    pub fn new(d: Device) -> br::Result<Self> {
        br::FenceObject::new(d, &br::FenceCreateInfo::new(0)).map(Self::Unsignaled)
    }
}
impl<Fence: br::FenceMut> StateFence<Fence> {
    /// Set state to Signaled
    ///
    /// # Safety
    /// Internal state must be coherent with background API
    pub const unsafe fn signal(&mut self) {
        let obj = core::ptr::read(match self {
            StateFence::Signaled(f) | StateFence::Unsignaled(f) => f as *const _,
        });
        core::mem::forget(core::mem::replace(self, StateFence::Signaled(obj)));
    }

    /// Set state to Unsignaled
    ///
    /// # Safety
    /// must be coherent with background API
    pub const unsafe fn unsignal(&mut self) {
        let obj = core::ptr::read(match self {
            StateFence::Signaled(f) | StateFence::Unsignaled(f) => f as *const _,
        });
        core::mem::forget(core::mem::replace(self, StateFence::Unsignaled(obj)));
    }

    /// Wait for a fence if it is in Signaled state
    ///
    /// After waiting, a fence will be resetted and state will be set to Unsignaled state
    pub fn wait(&mut self) -> br::Result<()> {
        if let StateFence::Signaled(ref mut f) = *self {
            f.wait()?;
            f.reset()?;
        }
        unsafe {
            self.unsignal();
        }

        Ok(())
    }

    /// Return internal fence object
    pub const fn inner_mut(&mut self) -> &mut Fence {
        match self {
            Self::Signaled(f) | Self::Unsignaled(f) => f,
        }
    }
}
