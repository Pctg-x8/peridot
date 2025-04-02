use bedrock::{self as br};
use std::ops::{Deref, DerefMut};

use super::{Graphics, VulkanGfx};

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum CBSubmissionType {
    Graphics,
    Transfer,
}

#[repr(transparent)]
pub struct CommandBundleBufferRef<'a, Device: br::Device>(
    br::vk::VkCommandBuffer,
    core::marker::PhantomData<&'a CommandBundle<Device>>,
);
impl<Device: br::Device> br::VkHandle for CommandBundleBufferRef<'_, Device> {
    type Handle = br::vk::VkCommandBuffer;

    fn native_ptr(&self) -> Self::Handle {
        self.0
    }
}
impl<Device: br::Device> br::CommandBuffer for CommandBundleBufferRef<'_, Device> {}

#[repr(transparent)]
pub struct CommandBundleBufferRefMut<'a, Device: br::Device>(
    br::vk::VkCommandBuffer,
    core::marker::PhantomData<&'a mut CommandBundle<Device>>,
);
impl<Device: br::Device> br::VkHandle for CommandBundleBufferRefMut<'_, Device> {
    type Handle = br::vk::VkCommandBuffer;

    fn native_ptr(&self) -> Self::Handle {
        self.0
    }
}
impl<Device: br::Device> br::VkHandleMut for CommandBundleBufferRefMut<'_, Device> {
    fn native_ptr_mut(&mut self) -> Self::Handle {
        self.0
    }
}
impl<Device: br::Device> br::CommandBuffer for CommandBundleBufferRefMut<'_, Device> {}
impl<Device: br::Device> br::CommandBufferMut for CommandBundleBufferRefMut<'_, Device> {}

pub struct CommandBundleBufferIter<'a, Device: br::Device>(
    &'a [br::vk::VkCommandBuffer],
    usize,
    core::marker::PhantomData<&'a CommandBundle<Device>>,
);
impl<'a, Device: br::Device> Iterator for CommandBundleBufferIter<'a, Device> {
    type Item = CommandBundleBufferRef<'a, Device>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.1 >= self.0.len() {
            None
        } else {
            let buffer = CommandBundleBufferRef(self.0[self.1], core::marker::PhantomData);
            self.1 += 1;
            Some(buffer)
        }
    }
}

pub struct CommandBundleBufferIterMut<'a, Device: br::Device>(
    &'a mut [br::vk::VkCommandBuffer],
    usize,
    core::marker::PhantomData<&'a mut CommandBundle<Device>>,
);
impl<'a, Device: br::Device> Iterator for CommandBundleBufferIterMut<'a, Device> {
    type Item = CommandBundleBufferRefMut<'a, Device>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.1 >= self.0.len() {
            None
        } else {
            let buffer = CommandBundleBufferRefMut(self.0[self.1], core::marker::PhantomData);
            self.1 += 1;
            Some(buffer)
        }
    }
}

pub struct CommandBundle<Device: br::Device> {
    pub buffers: Vec<br::vk::VkCommandBuffer>,
    pub pool: br::vk::VkCommandPool,
    pub device: Device,
}
unsafe impl<Device: br::Device + Sync> Sync for CommandBundle<Device> {}
unsafe impl<Device: br::Device + Send> Send for CommandBundle<Device> {}
impl<Device: br::Device> Drop for CommandBundle<Device> {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_command_pool(self.device.native_ptr(), self.pool, None);
        }
    }
}
impl CommandBundle<VulkanGfx> {
    pub fn new(g: &Graphics, submission_type: CBSubmissionType, count: usize) -> br::Result<Self> {
        let qf = match submission_type {
            CBSubmissionType::Graphics => g.graphics_queue.family,
            CBSubmissionType::Transfer => g.graphics_queue.family,
        };
        let cp = unsafe {
            br::vkfn_wrapper::create_command_pool(
                g.gfx_device.0.device,
                &br::CommandPoolCreateInfo::new(qf),
                None,
            )?
        };
        let mut buffers = Vec::with_capacity(count);
        unsafe {
            buffers.set_len(buffers.capacity());
        }
        match unsafe {
            br::vkfn_wrapper::allocate_command_buffers(
                g.gfx_device.0.device,
                &br::CommandBufferAllocateInfo::new(
                    &mut br::VkHandleRefMut::dangling(cp),
                    count as _,
                    br::CommandBufferLevel::Primary,
                ),
                &mut buffers,
            )
        } {
            Ok(_) => (),
            Err(e) => {
                unsafe {
                    br::vkfn_wrapper::destroy_command_pool(g.gfx_device.0.device, cp, None);
                }

                return Err(e);
            }
        }

        Ok(Self {
            buffers,
            pool: cp,
            device: g.gfx_device.clone(),
        })
    }
}
impl<Device: br::Device> CommandBundle<Device> {
    #[inline]
    pub unsafe fn reset(&mut self) -> br::Result<()> {
        unsafe {
            br::vkfn_wrapper::reset_command_pool(
                self.device.native_ptr(),
                self.pool,
                br::CommandPoolResetFlags::RELEASE_RESOURCES,
            )
        }
    }

    #[inline]
    pub fn iter(&self) -> CommandBundleBufferIter<Device> {
        CommandBundleBufferIter(&self.buffers, 0, core::marker::PhantomData)
    }

    #[inline]
    pub fn iter_mut(&mut self) -> CommandBundleBufferIterMut<Device> {
        CommandBundleBufferIterMut(&mut self.buffers, 0, core::marker::PhantomData)
    }

    #[inline]
    pub fn nth_ref(&self, n: usize) -> CommandBundleBufferRef<Device> {
        CommandBundleBufferRef(self.buffers[n], core::marker::PhantomData)
    }

    #[inline]
    pub fn synchronized_nth(&mut self, n: usize) -> br::SynchronizedCommandBuffer {
        // self.0は必ずself.1から生成されてるのでsafe
        unsafe {
            br::SynchronizedCommandBuffer::new_unchecked(
                br::VkHandleRefMut::dangling(self.pool),
                br::VkHandleRefMut::dangling(self.buffers[n]),
            )
        }
    }
}

pub struct LocalCommandBundle<
    'p,
    CommandBuffer: br::CommandBufferMut,
    CommandPool: br::CommandPoolMut + 'p,
>(pub Vec<CommandBuffer>, pub &'p mut CommandPool);
impl<'p, CommandBuffer: br::CommandBufferMut, CommandPool: br::CommandPoolMut + 'p> Drop
    for LocalCommandBundle<'p, CommandBuffer, CommandPool>
{
    fn drop(&mut self) {
        unsafe {
            self.1.free(
                &self
                    .0
                    .iter_mut()
                    .map(|x| x.as_transparent_ref_mut())
                    .collect::<Vec<_>>()[..],
            );
        }
    }
}
impl<'p, CommandBuffer: br::CommandBufferMut, CommandPool: br::CommandPoolMut + 'p> Deref
    for LocalCommandBundle<'p, CommandBuffer, CommandPool>
{
    type Target = [CommandBuffer];

    fn deref(&self) -> &[CommandBuffer] {
        &self.0
    }
}
impl<'p, CommandBuffer: br::CommandBufferMut, CommandPool: br::CommandPoolMut + 'p> DerefMut
    for LocalCommandBundle<'p, CommandBuffer, CommandPool>
{
    fn deref_mut(&mut self) -> &mut [CommandBuffer] {
        &mut self.0
    }
}
