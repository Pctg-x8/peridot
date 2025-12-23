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

@_cdecl("nslog_utf8")
func nslogUtf8(ptr: UnsafePointer<UInt8>, len: size_t) {
    NSLog(String(bytes: UnsafeBufferPointer(start: ptr, count: len), encoding: .utf8)!)
}

func captionbarText() -> String? {
    var len = 0
    let p = withUnsafeMutablePointer(to: &len) { ptr in captionbar_text(ptr) }
    
    return String(
        bytes: UnsafeBufferPointer(
            start: unsafeBitCast(p, to: UnsafePointer<UInt8>.self),
            count: len
        ),
        encoding: .utf8
    )
}

@_cdecl("acquire_layer_size")
func acquireLayerSize(
    layer: UnsafeRawPointer,
    width: UnsafeMutablePointer<UInt32>,
    height: UnsafeMutablePointer<UInt32>,
) {
    let rect = unsafeBitCast(layer, to: CALayer.self).contentsRect
    
    width.pointee = UInt32(rect.size.width)
    height.pointee = UInt32(rect.size.height)
}

@_cdecl("nsapp_reply_should_terminate")
func nsapp_reply_should_terminate() {
    NSApplication.shared.reply(toApplicationShouldTerminate: true)
}

@_cdecl("nsbundle_path_for_resource")
func nsbundle_path_for_resource(
    path: UnsafePointer<UInt8>,
    pathLength: size_t,
    ext: UnsafePointer<UInt8>,
    extLength: size_t,
    outPath: UnsafeMutablePointer<UInt8>,
    outPathLength: UnsafeMutablePointer<size_t>
) -> Bool {
    guard let path = Bundle.main.path(
        forResource: String(
            bytes: UnsafeBufferPointer(start: path, count: pathLength),
            encoding: .utf8
        ),
        ofType: String(
            bytes: UnsafeBufferPointer(start: ext, count: extLength),
            encoding: .utf8
        )
    ) else {
        outPathLength.pointee = 0
        return true
    }
    
    let pathData = Data(path.utf8)
    if outPathLength.pointee < pathData.count {
        // insufficient storage
        outPathLength.pointee = pathData.count
        return false
    } else {
        pathData.copyBytes(to: outPath, count: pathData.count)
        outPathLength.pointee = pathData.count
        return true
    }
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
