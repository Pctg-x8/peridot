//
//  NativeInterface.swift
//  peridot-cradle
//
//  Created by S.Percentage on 2018/12/07.
//  Copyright © 2018 S.Percentage. All rights reserved.
//

import Foundation
import Cocoa

final class NativeGameEngine {
    private var p: OpaquePointer
    
    init(forView v: UnsafeMutablePointer<NSView>) {
        self.p = launch_game(UnsafeMutablePointer(v))
    }
    deinit { terminate_game(self.p) }
}
