#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    void (*onResize)(void* callerContext, double width, double height);
    void (*onPointerDown)(void* callerContext, double x, double y);
    void (*onPointerMove)(void* callerContext, double x, double y);
    void (*onPointerUp)(void* callerContext);
} WindowLinkCallbacks;

void rs_launch();

#ifdef __cplusplus
}
#endif
