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
    deinit {
        NSLog("GameEngine Terminating")
        terminate_game(self.p)
        NSLog("GameEngine Terminated")
    }
    
    func update() { update_game(self.p) }
    func resize(_ newSize: NSSize) {
        resize_game(self.p, UInt32(newSize.width), UInt32(newSize.height))
    }
    
    func handleCharacterKeyDown(character: UInt8) {
        handle_character_keydown(self.p, character)
    }
    func handleCharacterKeyUp(character: UInt8) {
        handle_character_keyup(self.p, character)
    }
    func handleKeymodDown(code: UInt8) {
        handle_keymod_down(self.p, code)
    }
    func handleKeymodUp(code: UInt8) {
        handle_keymod_up(self.p, code)
    }
    
    static func captionbarText() -> NSString? {
        let p = captionbar_text()
        return p.map { x in Unmanaged<NSString>.fromOpaque(x).takeUnretainedValue() }
    }
}

@_cdecl("nsbundle_path_for_resource")
func nsbundle_path_for_resource(path: NSString, ext: NSString) -> UnsafeMutableRawPointer? {
    guard let path = Bundle.main.path(forResource: path as String, ofType: ext as String) else { return nil }
    return Unmanaged.passRetained(path as NSString).toOpaque()
}

@_cdecl("nsscreen_backing_scale_factor")
func nsscreen_backing_scale_factor() -> Float32 {
    guard let mainScreen = NSScreen.main else { return 0.0 }
    return Float32(mainScreen.backingScaleFactor)
}
