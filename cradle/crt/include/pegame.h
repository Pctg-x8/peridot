#ifndef PERIDOT_GAME_KERNEL_CRT_INTERFACE_INCLUDED
#define PERIDOT_GAME_KERNEL_CRT_INTERFACE_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

#include <vulkan.h>

typedef struct _PeCrtRenderTargetInfo
{
    const char* vk_surface_extension_name;
    void* surface_factory_context_ptr;
    VkSurface (*surface_factory)(void* context, VkInstance instance);
    void (*get_current_extent)(void* context, uint32_t* width, uint32_t* height);
} PeCrtRenderTargetInfo;
typedef struct _PeCrtRuntimeInfo
{
    const PeCrtRenderTargetInfo* render_target_info;
    const char* asset_base_path;
} PeCrtRuntimeInfo;

typedef struct _PeCrtGameCore* PeCrtGameCore;

PeCrtGameCore pecrtInitGame(const PeCrtRuntimeInfo* rt_info);
void pecrtEndGame(PeCrtGameCore core);
void pecrtNextFrame(PeCrtGameCore core);

#ifdef __cplusplus
}
#endif

#endif
