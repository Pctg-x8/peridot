//! State Tracked Objects

use bedrock as br;
use br::Waitable;

/// State-tracked Fence
pub enum StateFence { Signaled(br::Fence), Unsignaled(br::Fence) }
impl StateFence {
    /// Create a fence with Unsignaled state
    pub fn new(d: &br::Device) -> br::Result<Self> {
        br::Fence::new(d, false).map(StateFence::Unsignaled)
    }

    /// Set state to Signaled
    /// 
    /// # Safety
    /// Internal state must be coherent with background API
    pub unsafe fn signal(&mut self) {
        let obj = std::ptr::read(match self { StateFence::Signaled(f) | StateFence::Unsignaled(f) => f as *const _ });
        std::mem::forget(std::mem::replace(self, StateFence::Signaled(obj)));
    }
    /// Set state to Unsignaled
    ///
    /// # Safety
    /// must be coherent with background API
    unsafe fn unsignal(&mut self) {
        let obj = std::ptr::read(match self { StateFence::Signaled(f) | StateFence::Unsignaled(f) => f as *const _ });
        std::mem::forget(std::mem::replace(self, StateFence::Unsignaled(obj)));
    }

    /// Wait for a fence if it is in Signaled state
    /// 
    /// After waiting, a fence will be resetted and state will be set to Unsignaled state
    pub fn wait(&mut self) -> br::Result<()> {
        if let StateFence::Signaled(ref f) = *self { f.wait()?; f.reset()?; }
        unsafe { self.unsignal(); } return Ok(());
    }

    /// Return internal fence object
    pub fn object(&self) -> &br::Fence {
        match *self { StateFence::Signaled(ref f) | StateFence::Unsignaled(ref f) => f }
    }
}
