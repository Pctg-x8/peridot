//
//  peridot-cradle-Bridging-Header.h
//  peridot-cradle
//
//  Created by S.Percentage on 2018/12/02.
//  Copyright © 2018 S.Percentage. All rights reserved.
//

#ifndef peridot_cradle_Bridging_Header_h
#define peridot_cradle_Bridging_Header_h

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    void (*terminate)(void* context_ptr, void* swift_context);
    void (*update)(void* context_ptr);
    void (*resize)(void* context_ptr, uint32_t w, uint32_t h);
    void (*handle_character_keydown)(void* context_ptr, uint8_t character);
    void (*handle_character_keyup)(void* context_ptr, uint8_t character);
    void (*handle_keymod_down)(void* context_ptr, uint8_t code);
    void (*handle_keymod_up)(void* context_ptr, uint8_t code);
    void (*handle_mouse_button_down)(void* context_ptr, uint8_t index);
    void (*handle_mouse_button_up)(void* context_ptr, uint8_t index);
    void (*report_mouse_move_abs)(void* context_ptr, float x, float y);
    void (*poll_usercode_task)(void* context_ptr);
} GameDriverCallbacks;

const uint8_t KEYMOD_SHIFT = 1;
const uint8_t KEYMOD_OPTION = 2;
const uint8_t KEYMOD_CONTROL = 3;
const uint8_t KEYMOD_COMMAND = 4;
const uint8_t KEYMOD_CAPSLOCK = 5;

void launch_game(void* swift_context, void* viewptr);
const char* captionbar_text(size_t* length);

typedef void (*AudioFormatCallback)(void* context, uint32_t channels, double sample_rate);
typedef uint8_t (*AudioRenderCallback)(void* context, uint32_t frame_count, void* buffer);

#ifdef __cplusplus
}
#endif

#endif /* peridot_cradle_Bridging_Header_h */
