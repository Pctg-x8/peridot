//
//  peridot-cradle-Bridging-Header.h
//  peridot-cradle
//
//  Created by S.Percentage on 2018/12/02.
//  Copyright © 2018 S.Percentage. All rights reserved.
//

#ifndef peridot_cradle_Bridging_Header_h
#define peridot_cradle_Bridging_Header_h

struct GameRun;

struct GameRun* launch_game(void* viewptr);
void terminate_game(struct GameRun* engineptr);

#endif /* peridot_cradle_Bridging_Header_h */
