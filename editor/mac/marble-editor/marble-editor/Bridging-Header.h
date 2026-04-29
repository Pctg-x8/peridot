#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

const uint8_t MouseButtonLeft = 0;
const uint8_t MouseButtonRight = 1;

typedef struct WindowLink_* WindowLink;
typedef struct {
    void (*destructor)(void* callerContext);
    void (*onWindowClose)(void* callerContext, WindowLink window);
    void (*onResize)(void* callerContext, WindowLink window, double width, double height);
    void (*onPointerDown)(void* callerContext, WindowLink window, double x, double y, uint8_t button);
    void (*onPointerMove)(void* callerContext, WindowLink window, double x, double y);
    void (*onPointerUp)(void* callerContext, WindowLink window, uint8_t button);
    void (*onKeyDown)(void* callerContext, WindowLink window, uint16_t code, uint32_t modifierFlags);
    void (*onKeyDownWithChar)(void* callerContext, WindowLink window, uint16_t code, uint32_t modifierFlags, uint32_t ch);
    void (*onKeyUp)(void* callerContext, WindowLink window, uint16_t code, uint32_t modifierFlags);
    void (*onKeyFocusStateChanged)(void* callerContext, WindowLink window, uint8_t focused);
} WindowLinkCallbacks;

typedef struct ContextMenuSurface_* ContextMenuSurface;
typedef struct {
    void (*onPointerDown)(ContextMenuSurface sender, double x, double y, uint8_t button);
    void (*onPointerMove)(ContextMenuSurface sender, double x, double y);
    void (*onPointerUp)(ContextMenuSurface sender, uint8_t button);
    void (*onPointerLeave)(ContextMenuSurface sender);
} ContextMenuSurfaceCallbacks;

typedef void (*UnboundCallback)(void* callerContext);
typedef void (*ContextMenuGlobalClickCallback)(void* callerContext, uint8_t onContextMenuSurface);

void rs_launch();

#ifdef __cplusplus
}
#endif
