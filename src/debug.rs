use bedrock::vk::*;
use libc::c_void;
use std::ffi::CStr;
use log::*;

pub extern "system" fn debug_utils_out(
    severity: VkDebugUtilsMessageSeverityFlagBitsEXT,
    ty: VkDebugUtilsMessageTypeFlagsEXT,
    callback_data: *const VkDebugUtilsMessengerCallbackDataEXT,
    _user_data: *mut c_void
) -> VkBool32 {
    let bt = std::backtrace::Backtrace::capture();

    let callback_data = unsafe { callback_data.as_ref().expect("null callback data received at debug_utils_out") };
    let msg = unsafe { CStr::from_ptr(callback_data.pMessage).to_str().unwrap_or("message has illegal character") };

    let mut tys = Vec::with_capacity(3);
    if (ty & VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT) != 0 {
        tys.push("GENERAL");
    }
    if (ty & VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT) != 0 {
        tys.push("VALIDATION");
    }
    if (ty & VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT) != 0 {
        tys.push("PERFORMANCE");
    }
    let tystr = tys.join("/");

    match severity {
        VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT => {
            error!("vkerr[{tystr}]: {msg}");
            error!("  id: {}", callback_data.messageIdNumber);
            error!(
                "  object-counts: q={} c={} o={}",
                callback_data.queueLabelCount,
                callback_data.cmdBufLabelCount,
                callback_data.objectCount
            );
            error!("  *backtrace*\n{bt}");
        },
        VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT => {
            warn!("vkwarn[{tystr}]: {msg}");
            warn!("  id: {}", callback_data.messageIdNumber);
            warn!(
                "  object-counts: q={} c={} o={}",
                callback_data.queueLabelCount,
                callback_data.cmdBufLabelCount,
                callback_data.objectCount
            );
            warn!("  *backtrace*\n{bt}");
        },
        VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT => {
            info!("vkinfo[{tystr}]: {msg}");
            info!("  id: {}", callback_data.messageIdNumber);
            info!(
                "  object-counts: q={} c={} o={}",
                callback_data.queueLabelCount,
                callback_data.cmdBufLabelCount,
                callback_data.objectCount
            );
            info!("  *backtrace*\n{bt}");
        },
        VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT => {
            trace!("vk[{tystr}]: {msg}");
            trace!("  id: {}", callback_data.messageIdNumber);
            trace!(
                "  object-counts: q={} c={} o={}",
                callback_data.queueLabelCount,
                callback_data.cmdBufLabelCount,
                callback_data.objectCount
            );
            trace!("  *backtrace*\n{bt}");
        },
        _ => unreachable!("unknown severity flag")
    }

    VK_FALSE
}
