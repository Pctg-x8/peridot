use bedrock as br;
use br::{CommandBuffer, CommandPool, Device};
use std::ops::{Deref, DerefMut};

use super::{DeviceObject, Graphics};

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum CBSubmissionType {
    Graphics,
    Transfer,
}

pub struct CommandBundle<Device: br::Device>(
    pub Vec<br::CommandBufferObject<Device>>,
    pub br::CommandPoolObject<Device>,
);
impl<Device: br::Device> Deref for CommandBundle<Device> {
    type Target = [br::CommandBufferObject<Device>];

    fn deref(&self) -> &[br::CommandBufferObject<Device>] {
        &self.0
    }
}
impl<Device: br::Device> DerefMut for CommandBundle<Device> {
    fn deref_mut(&mut self) -> &mut [br::CommandBufferObject<Device>] {
        &mut self.0
    }
}
impl<Device: br::Device> Drop for CommandBundle<Device> {
    fn drop(&mut self) {
        unsafe {
            self.1.free(&self.0[..]);
        }
    }
}
impl CommandBundle<DeviceObject> {
    pub fn new(g: &Graphics, submission_type: CBSubmissionType, count: usize) -> br::Result<Self> {
        let qf = match submission_type {
            CBSubmissionType::Graphics => g.graphics_queue.family,
            CBSubmissionType::Transfer => g.graphics_queue.family,
        };
        let mut cp = br::CommandPoolBuilder::new(qf).create(g.device.clone())?;

        Ok(Self(cp.alloc(count as _, true)?, cp))
    }
}
impl<Device: br::Device> CommandBundle<Device> {
    #[inline]
    pub fn reset(&mut self) -> br::Result<()> {
        self.1.reset(true)
    }

    #[inline]
    pub unsafe fn synchronized_nth(
        &mut self,
        n: usize,
    ) -> br::SynchronizedCommandBuffer<br::CommandPoolObject<Device>, br::CommandBufferObject<Device>>
    {
        self.0[n].synchronize_with(&mut self.1)
    }
}

pub struct LocalCommandBundle<
    'p,
    CommandBuffer: br::CommandBuffer,
    CommandPool: br::CommandPool + br::VkHandleMut + 'p,
>(pub Vec<CommandBuffer>, pub &'p mut CommandPool);
impl<'p, CommandBuffer: br::CommandBuffer, CommandPool: br::CommandPool + br::VkHandleMut + 'p>
    Deref for LocalCommandBundle<'p, CommandBuffer, CommandPool>
{
    type Target = [CommandBuffer];

    fn deref(&self) -> &[CommandBuffer] {
        &self.0
    }
}
impl<'p, CommandBuffer: br::CommandBuffer, CommandPool: br::CommandPool + br::VkHandleMut + 'p>
    DerefMut for LocalCommandBundle<'p, CommandBuffer, CommandPool>
{
    fn deref_mut(&mut self) -> &mut [CommandBuffer] {
        &mut self.0
    }
}
impl<'p, CommandBuffer: br::CommandBuffer, CommandPool: br::CommandPool + br::VkHandleMut + 'p> Drop
    for LocalCommandBundle<'p, CommandBuffer, CommandPool>
{
    fn drop(&mut self) {
        unsafe {
            self.1.free(&self.0[..]);
        }
    }
}
