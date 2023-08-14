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
        d.new_fence(false).map(Self::Unsignaled)
    }
}
impl<Fence: br::Fence + br::VkHandleMut> StateFence<Fence> {
    #[allow(dead_code)]
    /// Wrap a unsignaled fence
    pub const unsafe fn wrap_unsignaled(f: Fence) -> Self {
        Self::Unsignaled(f)
    }

    /// Set state to Signaled
    ///
    /// # Safety
    /// Internal state must be coherent with background API
    pub unsafe fn signal(&mut self) {
        let obj = std::ptr::read(match self {
            StateFence::Signaled(f) | StateFence::Unsignaled(f) => f as *const _,
        });
        std::mem::forget(std::mem::replace(self, StateFence::Signaled(obj)));
    }

    /// Set state to Unsignaled
    ///
    /// # Safety
    /// must be coherent with background API
    unsafe fn unsignal(&mut self) {
        let obj = std::ptr::read(match self {
            StateFence::Signaled(f) | StateFence::Unsignaled(f) => f as *const _,
        });
        std::mem::forget(std::mem::replace(self, StateFence::Unsignaled(obj)));
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
        return Ok(());
    }

    /// Return internal fence object
    pub fn inner_mut(&mut self) -> &mut Fence {
        match self {
            Self::Signaled(f) | Self::Unsignaled(f) => f,
        }
    }
}
