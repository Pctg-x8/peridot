use bedrock as br;
use libc::{size_t, c_char, c_void};
use std::ffi::CStr;

pub struct DebugReport(br::DebugReportCallback);
impl DebugReport
{
    pub fn new(instance: &br::Instance) -> br::Result<Self>
    {
        let obj = br::DebugReportCallbackBuilder::new(instance, Self::debug_output)
            .report_error().report_warning().report_performance_warning().create()?;
        return Ok(DebugReport(obj));
    }

    extern "system" fn debug_output(flags: br::vk::VkDebugReportFlagsEXT, object_type: br::vk::VkDebugReportObjectTypeEXT,
        object: u64, location: size_t, message_code: i32, layer_prefix: *const c_char,
        message: *const c_char, _: *mut c_void) -> br::vk::VkBool32
    {
        let (layer_prefix, msg) = unsafe { (CStr::from_ptr(layer_prefix), CStr::from_ptr(message)) };
        if (flags & br::vk::VK_DEBUG_REPORT_ERROR_BIT_EXT) != 0
        {
            error!("vkerr: {}", msg.to_str().unwrap());
            error!("  Code: {}", message_code);
            error!("  Object: {} type={}", object, object_type);
            error!("  Location: {}", location);
            error!("  Layer-Prefix: {}", layer_prefix.to_str().unwrap());
        }
        else if (flags & br::vk::VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT) != 0
        {
            warn!("vkwarn_perf: {}", msg.to_str().unwrap());
            warn!("  Code: {}", message_code);
            warn!("  Object: {} type={}", object, object_type);
            warn!("  Location: {}", location);
            warn!("  Layer-Prefix: {}", layer_prefix.to_str().unwrap());
        }
        else if (flags & br::vk::VK_DEBUG_REPORT_WARNING_BIT_EXT) != 0
        {
            warn!("vkwarn: {}", msg.to_str().unwrap());
            warn!("  Code: {}", message_code);
            warn!("  Object: {} type={}", object, object_type);
            warn!("  Location: {}", location);
            warn!("  Layer-Prefix: {}", layer_prefix.to_str().unwrap());
        }
        else
        {
            info!("vkinfo: {}", msg.to_str().unwrap());
            info!("  Code: {}", message_code);
            info!("  Object: {} type={}", object, object_type);
            info!("  Location: {}", location);
            info!("  Layer-Prefix: {}", layer_prefix.to_str().unwrap());
        }
        ((flags & br::vk::VK_DEBUG_REPORT_ERROR_BIT_EXT) != 0) as _
    }
}
