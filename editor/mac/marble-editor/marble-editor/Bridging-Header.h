#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

struct WindowLink;
typedef struct {
    void (*onResize)(void* callerContext, WindowLink* window, double width, double height);
    void (*onPointerDown)(void* callerContext, WindowLink* window, double x, double y);
    void (*onPointerMove)(void* callerContext, WindowLink* window, double x, double y);
    void (*onPointerUp)(void* callerContext, WindowLink* window);
} WindowLinkCallbacks;

void rs_launch();

#ifdef __cplusplus
}
#endif
