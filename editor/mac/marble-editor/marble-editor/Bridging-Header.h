#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    void (*onResize)(void* callerContext, uint32_t width, uint32_t height)
} WindowLinkCallbacks;

void rs_launch();

#ifdef __cplusplus
}
#endif
