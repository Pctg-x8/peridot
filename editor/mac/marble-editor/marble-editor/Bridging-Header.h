#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct WindowLink_* WindowLink;
typedef struct {
    void (*destructor)(void* callerContext);
    void (*onWindowClose)(void* callerContext, WindowLink window);
    void (*onResize)(void* callerContext, WindowLink window, double width, double height);
    void (*onPointerDown)(void* callerContext, WindowLink window, double x, double y);
    void (*onPointerMove)(void* callerContext, WindowLink window, double x, double y);
    void (*onPointerUp)(void* callerContext, WindowLink window);
    void (*onKeyDown)(void* callerContext, WindowLink window, uint16_t code, uint32_t modifierFlags);
    void (*onKeyDownWithChar)(void* callerContext, WindowLink window, uint16_t code, uint32_t modifierFlags, uint32_t ch);
    void (*onKeyUp)(void* callerContext, WindowLink window, uint16_t code, uint32_t modifierFlags);
    void (*onKeyFocusStateChanged)(void* callerContext, WindowLink window, uint8_t focused);
} WindowLinkCallbacks;

typedef void (*UnboundCallback)(void* callerContext);

void rs_launch();

#ifdef __cplusplus
}
#endif
