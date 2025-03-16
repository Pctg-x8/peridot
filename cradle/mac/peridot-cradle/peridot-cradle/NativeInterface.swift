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
    
    init(forLayer layer: CAMetalLayer, on initializationContext: UnsafeMutableRawPointer) {
        self.p = launch_game(initializationContext, unsafeBitCast(layer, to: UnsafeMutablePointer.self))
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
    
    func handleMouseButtonDown(index: UInt8) { handle_mouse_button_down(self.p, index) }
    func handleMouseButtonUp(index: UInt8) { handle_mouse_button_up(self.p, index) }
    func reportMouseMove(x: Float, y: Float) { report_mouse_move_abs(self.p, x, y) }
    
    static func captionbarText() -> NSString? {
        let p = captionbar_text()
        return p.map { x in Unmanaged<NSString>.fromOpaque(x).takeUnretainedValue() }
    }
}

@_cdecl("nsapp_reply_should_terminate")
func nsapp_reply_should_terminate() {
    NSApplication.shared.reply(toApplicationShouldTerminate: true)
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

@_cdecl("obtain_mouse_pointer_position")
func obtain_mouse_pointer_position(
    viewptr: UnsafeMutableRawPointer,
    x: UnsafeMutablePointer<Float32>,
    y: UnsafeMutablePointer<Float32>
) {
    let v = unsafeBitCast(viewptr, to: PeridotRenderableView.self)
    if let p = v.window?.mouseLocationOutsideOfEventStream {
        let h = v.frame.height
        var pl = v.convert(p, from: nil)
        // Note: MacBook Pro 16inch 2019だとなぜかpの時点で5.0だけずれてる
        pl.y += 5.0
        x.pointee = Float32(pl.x) * nsscreen_backing_scale_factor()
        y.pointee = Float32(h - pl.y) * nsscreen_backing_scale_factor()
    }
}

@_cdecl("give_game_driver_callbacks")
func give_game_driver_callbacks(initializationContext: UnsafeMutableRawPointer, callbacks: UnsafeMutablePointer<GameDriverCallbacks>, contextPtr: UnsafeMutableRawPointer) {
    unsafeBitCast(initializationContext, to: PeridotRenderableViewController.self).setGameDriverCallbacks(callbacks, contextPtr: contextPtr)
}
