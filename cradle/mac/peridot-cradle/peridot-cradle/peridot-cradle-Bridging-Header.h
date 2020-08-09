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

typedef struct GameRun_ GameRun;

GameRun* launch_game(void* viewptr);
void terminate_game(GameRun* engineptr);
void update_game(GameRun* engineptr);
void resize_game(GameRun* engineptr, uint32_t w, uint32_t h);

void handle_character_keydown(GameRun* engineptr, uint8_t character);
void handle_character_keyup(GameRun* engineptr, uint8_t character);

void* captionbar_text();

#endif /* peridot_cradle_Bridging_Header_h */
