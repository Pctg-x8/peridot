import Foundation
import Cocoa

struct NativeGameDriver {
    private let callbacks: UnsafeMutablePointer<GameDriverCallbacks>
    private let contextPtr: UnsafeMutableRawPointer
    
    init(
        callbacks: UnsafeMutablePointer<GameDriverCallbacks>,
        contextPtr: UnsafeMutableRawPointer
    ) {
        self.callbacks = callbacks
        self.contextPtr = contextPtr
    }
    
    func terminate() {
        self.callbacks.pointee.terminate(self.contextPtr)
    }
    
    func update() {
        self.callbacks.pointee.update(self.contextPtr)
    }
    
    func resize(_ size: NSSize) {
        self.callbacks.pointee.resize(self.contextPtr, UInt32(size.width), UInt32(size.height))
    }
    
    func handleKeyDown(character c: UniChar) {
        self.callbacks.pointee.handle_character_keydown(self.contextPtr, UInt8(c))
    }
    
    func handleKeyUp(character c: UniChar) {
        self.callbacks.pointee.handle_character_keyup(self.contextPtr, UInt8(c))
    }
    
    func handleKeyDown(mod code: UInt8) {
        self.callbacks.pointee.handle_keymod_down(self.contextPtr, code)
    }
    
    func handleKeyUp(mod code: UInt8) {
        self.callbacks.pointee.handle_keymod_up(self.contextPtr, code)
    }
    
    func handleMouseButtonDown(_ index: UInt8) {
        self.callbacks.pointee.handle_mouse_button_down(self.contextPtr, index)
    }
    
    func handleMouseButtonUp(_ index: UInt8) {
        self.callbacks.pointee.handle_mouse_button_up(self.contextPtr, index)
    }
    
    func reportMouseMoveAbs(x: Float, y: Float) {
        self.callbacks.pointee.report_mouse_move_abs(self.contextPtr, x, y)
    }
    
    func pollUsercodeTask() {
        self.callbacks.pointee.poll_usercode_task(self.contextPtr)
    }
}

func captionbarText() -> NSString? {
    let p = captionbar_text()
    return p.map { x in Unmanaged<NSString>.fromOpaque(x).takeUnretainedValue() }
}

@_cdecl("nsapp_reply_should_terminate")
func nsapp_reply_should_terminate() {
    NSApplication.shared.reply(toApplicationShouldTerminate: true)
}

@_cdecl("nsbundle_path_for_resource")
func nsbundle_path_for_resource(path: NSString, ext: NSString) -> UnsafeMutableRawPointer? {
    guard let path = Bundle.main.path(forResource: path as String, ofType: ext as String) else {
        return nil
    }
    
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
func give_game_driver_callbacks(
    initializationContext: UnsafeMutableRawPointer,
    callbacks: UnsafeMutablePointer<GameDriverCallbacks>,
    contextPtr: UnsafeMutableRawPointer
) {
    unsafeBitCast(initializationContext, to: PeridotRenderableViewController.self)
        .nativeGameDriver = NativeGameDriver(callbacks: callbacks, contextPtr: contextPtr)
}

@_cdecl("schedule_usercode_task_polling")
func scheduleUsercodeTaskPolling(initializationContext: UnsafeMutableRawPointer) {
    DispatchQueue.main.async {
        unsafeBitCast(initializationContext, to: PeridotRenderableViewController.self)
            .nativeGameDriver?.pollUsercodeTask()
    }
}
