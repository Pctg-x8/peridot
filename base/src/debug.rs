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
    match x {
        VK_OBJECT_TYPE_UNKNOWN => "???".into(),
        VK_OBJECT_TYPE_INSTANCE => "Instance".into(),
        VK_OBJECT_TYPE_PHYSICAL_DEVICE => "PhysicalDevice".into(),
        VK_OBJECT_TYPE_DEVICE => "Device".into(),
        VK_OBJECT_TYPE_QUEUE => "Queue".into(),
        VK_OBJECT_TYPE_SEMAPHORE => "Semaphore".into(),
        VK_OBJECT_TYPE_COMMAND_BUFFER => "CommandBuffer".into(),
        VK_OBJECT_TYPE_FENCE => "Fence".into(),
        VK_OBJECT_TYPE_DEVICE_MEMORY => "DeviceMemory".into(),
        VK_OBJECT_TYPE_BUFFER => "Buffer".into(),
        VK_OBJECT_TYPE_IMAGE => "Image".into(),
        VK_OBJECT_TYPE_EVENT => "Event".into(),
        VK_OBJECT_TYPE_QUERY_POOL => "QueryPool".into(),
        VK_OBJECT_TYPE_BUFFER_VIEW => "BufferView".into(),
        VK_OBJECT_TYPE_IMAGE_VIEW => "ImageView".into(),
        VK_OBJECT_TYPE_SHADER_MODULE => "ShaderModule".into(),
        VK_OBJECT_TYPE_PIPELINE_CACHE => "PipelineCache".into(),
        VK_OBJECT_TYPE_PIPELINE_LAYOUT => "PipelineLayout".into(),
        VK_OBJECT_TYPE_RENDER_PASS => "RenderPass".into(),
        VK_OBJECT_TYPE_PIPELINE => "Pipeline".into(),
        VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT => "DescriptorSetLayout".into(),
        VK_OBJECT_TYPE_SAMPLER => "Sampler".into(),
        VK_OBJECT_TYPE_DESCRIPTOR_POOL => "DescriptorPool".into(),
        VK_OBJECT_TYPE_DESCRIPTOR_SET => "DescriptorSet".into(),
        VK_OBJECT_TYPE_FRAMEBUFFER => "Framebuffer".into(),
        VK_OBJECT_TYPE_COMMAND_POOL => "CommandPool".into(),
        _ => format!("UnknownObject#{x}").into(),
    }
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
        for x in unsafe { callback_data.queue_labels() } {
            if !first {
                s.push(',');
            }

            first = false;
            s.push_str(&unsafe { x.label_name_cstr().to_string_lossy() });
        }
        s.push(']');

        tracing::Span::current().record("queue_labels", &tracing::field::display(s));
    }

    if callback_data.cmdBufLabelCount > 0 {
        let mut s = String::new();
        s.push('[');
        let mut first = true;
        for x in unsafe { callback_data.cmd_buf_labels() } {
            if !first {
                s.push(',');
            }

            first = false;
            s.push_str(&unsafe { x.label_name_cstr().to_string_lossy() });
        }
        s.push(']');

        tracing::Span::current().record("cmdbuf_labels", &tracing::field::display(s));
    }

    if callback_data.objectCount > 0 {
        let mut s = String::new();
        s.push('[');
        let mut first = true;
        for x in unsafe { callback_data.objects() } {
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
            if let Some(n) = unsafe { x.object_name_cstr() } {
                s.push('(');
                s.push_str(&n.to_string_lossy());
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
