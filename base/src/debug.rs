use bedrock::vk::*;
use core::fmt::Write;
use libc::c_void;
use std::{borrow::Cow, ffi::CStr};
use tracing::*;

fn format_message_type(x: VkDebugUtilsMessageTypeFlagsEXT) -> String {
    let mut xs = Vec::with_capacity(3);
    if (x & VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT) != 0 {
        xs.push("GENERAL");
    }
    if (x & VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT) != 0 {
        xs.push("VALIDATION");
    }
    if (x & VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT) != 0 {
        xs.push("PERFORMANCE");
    }

    xs.join("/")
}

fn format_object_type(x: VkObjectType) -> Cow<'static, str> {
    if x == VK_OBJECT_TYPE_INSTANCE {
        return "Instance".into();
    }
    if x == VK_OBJECT_TYPE_PHYSICAL_DEVICE {
        return "PhysicalDevice".into();
    }
    if x == VK_OBJECT_TYPE_DEVICE {
        return "Device".into();
    }
    if x == VK_OBJECT_TYPE_QUEUE {
        return "Queue".into();
    }
    if x == VK_OBJECT_TYPE_SEMAPHORE {
        return "Semaphore".into();
    }
    if x == VK_OBJECT_TYPE_COMMAND_BUFFER {
        return "CommandBuffer".into();
    }
    if x == VK_OBJECT_TYPE_FENCE {
        return "Fence".into();
    }
    if x == VK_OBJECT_TYPE_DEVICE_MEMORY {
        return "DeviceMemory".into();
    }
    if x == VK_OBJECT_TYPE_BUFFER {
        return "Buffer".into();
    }
    if x == VK_OBJECT_TYPE_IMAGE {
        return "Image".into();
    }
    if x == VK_OBJECT_TYPE_EVENT {
        return "Event".into();
    }
    if x == VK_OBJECT_TYPE_QUERY_POOL {
        return "QueryPool".into();
    }
    if x == VK_OBJECT_TYPE_BUFFER_VIEW {
        return "BufferView".into();
    }
    if x == VK_OBJECT_TYPE_IMAGE_VIEW {
        return "ImageView".into();
    }
    if x == VK_OBJECT_TYPE_SHADER_MODULE {
        return "ShaderModule".into();
    }
    if x == VK_OBJECT_TYPE_PIPELINE_CACHE {
        return "PipelineCache".into();
    }
    if x == VK_OBJECT_TYPE_PIPELINE_LAYOUT {
        return "PipelineLayout".into();
    }
    if x == VK_OBJECT_TYPE_RENDER_PASS {
        return "RenderPass".into();
    }
    if x == VK_OBJECT_TYPE_PIPELINE {
        return "Pipeline".into();
    }
    if x == VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT {
        return "DescriptorSetLayout".into();
    }
    if x == VK_OBJECT_TYPE_SAMPLER {
        return "Sampler".into();
    }
    if x == VK_OBJECT_TYPE_DESCRIPTOR_POOL {
        return "DescriptorPool".into();
    }
    if x == VK_OBJECT_TYPE_DESCRIPTOR_SET {
        return "DescriptorSet".into();
    }
    if x == VK_OBJECT_TYPE_FRAMEBUFFER {
        return "Framebuffer".into();
    }
    if x == VK_OBJECT_TYPE_COMMAND_POOL {
        return "CommandPool".into();
    }

    format!("UnknownObject#{x}").into()
}

pub struct MessageIDFormatter<'d>(&'d VkDebugUtilsMessengerCallbackDataEXT);
impl core::fmt::Display for MessageIDFormatter<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if self.0.pMessageIdName.is_null() {
            // no message name
            return write!(f, "#{}", self.0.messageIdNumber);
        }

        match unsafe { CStr::from_ptr(self.0.pMessageIdName).to_str() } {
            Ok(x) => f.write_str(x),
            Err(_) => write!(f, "#{}", self.0.messageIdNumber),
        }
    }
}

pub extern "system" fn debug_utils_callback(
    severity: VkDebugUtilsMessageSeverityFlagBitsEXT,
    ty: VkDebugUtilsMessageTypeFlagsEXT,
    callback_data: *const VkDebugUtilsMessengerCallbackDataEXT,
    user_data: *mut c_void,
) -> VkBool32 {
    let bt = std::backtrace::Backtrace::capture();

    let Some(callback_data) = (unsafe { callback_data.as_ref() }) else {
        warn!("(null callback data received at debug_utils_out)");
        return VK_FALSE;
    };

    vk_log(severity, ty, callback_data, user_data, bt)
}

#[instrument(
    skip(severity, ty, callback_data, _user_data, bt),
    fields(
        ty = %format_message_type(ty),
        id = %MessageIDFormatter(callback_data),
        queue_labels,
        cmdbuf_labels,
        objects
    )
)]
fn vk_log(
    severity: VkDebugUtilsMessageSeverityFlagBitsEXT,
    ty: VkDebugUtilsMessageTypeFlagsEXT,
    callback_data: &VkDebugUtilsMessengerCallbackDataEXT,
    _user_data: *mut c_void,
    bt: std::backtrace::Backtrace,
) -> VkBool32 {
    if callback_data.queueLabelCount > 0 {
        let mut s = String::new();
        s.push('[');
        let mut first = true;
        for x in unsafe {
            core::slice::from_raw_parts(
                callback_data.pQueueLabels,
                callback_data.queueLabelCount as _,
            )
        } {
            if !first {
                s.push(',');
            }

            first = false;
            s.push_str(&unsafe { core::ffi::CStr::from_ptr(x.pLabelName).to_string_lossy() });
        }
        s.push(']');

        tracing::Span::current().record("queue_labels", &tracing::field::display(s));
    }

    if callback_data.cmdBufLabelCount > 0 {
        let mut s = String::new();
        s.push('[');
        let mut first = true;
        for x in unsafe {
            core::slice::from_raw_parts(
                callback_data.pCmdBufLabels,
                callback_data.cmdBufLabelCount as _,
            )
        } {
            if !first {
                s.push(',');
            }

            first = false;
            s.push_str(&unsafe { core::ffi::CStr::from_ptr(x.pLabelName).to_string_lossy() });
        }
        s.push(']');

        tracing::Span::current().record("cmdbuf_labels", &tracing::field::display(s));
    }

    if callback_data.objectCount > 0 {
        let mut s = String::new();
        s.push('[');
        let mut first = true;
        for x in unsafe {
            core::slice::from_raw_parts(callback_data.pObjects, callback_data.objectCount as _)
        } {
            if !first {
                s.push(',');
            }

            first = false;
            write!(
                &mut s,
                "{}@0x{:x}",
                format_object_type(x.objectType),
                x.objectHandle
            )
            .expect("formatting failed");
            if !x.pObjectName.is_null() {
                s.push('(');
                s.push_str(&unsafe { core::ffi::CStr::from_ptr(x.pObjectName).to_string_lossy() });
                s.push(')');
            }
        }
        s.push(']');

        tracing::Span::current().record("objects", &tracing::field::display(s));
    }

    let msg = match unsafe { CStr::from_ptr(callback_data.pMessage).to_str() } {
        Ok(x) => x,
        Err(e) => {
            warn!({ cause = ?e }, "(message has illegal character)");
            return VK_FALSE;
        }
    };

    match severity {
        VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT => {
            error!("{msg}\n*backtrace*\n{bt}");
        }
        VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT => {
            warn!("{msg}\n*backtrace*\n{bt}");
        }
        VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT => {
            info!("{msg}\n*backtrace*\n{bt}");
        }
        VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT => {
            trace!("{msg}\n*backtrace*\n{bt}");
        }
        _ => unreachable!("unknown severity flag"),
    }

    VK_FALSE
}
