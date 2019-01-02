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
    
    init(forView v: inout NSView) {
        self.p = launch_game(unsafeBitCast(v, to: UnsafeMutablePointer.self))
    }
    deinit { terminate_game(self.p) }
    
    func update() { update_game(self.p) }
}

@_cdecl("nsbundle_path_for_resource")
func nsbundle_path_for_resource(path: NSString, ext: NSString) -> UnsafeMutableRawPointer? {
    let path = Bundle.main.path(forResource: path as String, ofType: ext as String)
    if let p = path {
        return Unmanaged.passRetained(p as NSString).toOpaque()
    }
    else { return nil }
}
