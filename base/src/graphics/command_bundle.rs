use bedrock::{self as br, CommandBufferMut, CommandPoolMut};
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
        let mut cp =
            br::CommandPoolObject::new(g.device.clone(), &br::CommandPoolCreateInfo::new(qf))?;

        Ok(Self(
            br::CommandBufferObject::alloc(
                g.device.clone(),
                &br::CommandBufferAllocateInfo::new(
                    &mut cp,
                    count as _,
                    br::CommandBufferLevel::Primary,
                ),
            )?,
            cp,
        ))
    }
}
impl<Device: br::Device> CommandBundle<Device> {
    #[inline]
    pub unsafe fn reset(&mut self) -> br::Result<()> {
        self.1
            .reset(br::vk::VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT)
    }

    #[inline]
    pub fn synchronized_nth(
        &mut self,
        n: usize,
    ) -> br::SynchronizedCommandBuffer<br::CommandPoolObject<Device>, br::CommandBufferObject<Device>>
    {
        // self.0は必ずself.1から生成されてるのでsafe
        unsafe { self.0[n].synchronize_with(&mut self.1) }
    }
}

pub struct LocalCommandBundle<
    'p,
    CommandBuffer: br::CommandBuffer,
    CommandPool: br::CommandPoolMut + 'p,
>(pub Vec<CommandBuffer>, pub &'p mut CommandPool);
impl<'p, CommandBuffer: br::CommandBuffer, CommandPool: br::CommandPoolMut + 'p> Drop
    for LocalCommandBundle<'p, CommandBuffer, CommandPool>
{
    fn drop(&mut self) {
        unsafe {
            self.1.free(&self.0[..]);
        }
    }
}
impl<'p, CommandBuffer: br::CommandBuffer, CommandPool: br::CommandPoolMut + 'p> Deref
    for LocalCommandBundle<'p, CommandBuffer, CommandPool>
{
    type Target = [CommandBuffer];

    fn deref(&self) -> &[CommandBuffer] {
        &self.0
    }
}
impl<'p, CommandBuffer: br::CommandBuffer, CommandPool: br::CommandPoolMut + 'p> DerefMut
    for LocalCommandBundle<'p, CommandBuffer, CommandPool>
{
    fn deref_mut(&mut self) -> &mut [CommandBuffer] {
        &mut self.0
    }
}
