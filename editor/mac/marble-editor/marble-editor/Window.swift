import Cocoa
import os

final class WindowLink : NSWindow {
    private let windowDelegate: Delegate
    private var callbacks: CallbackSet? = nil
    let mainView = MainView()
    
    init(_ flags: CreationFlags) {
        let styleMask: StyleMask = [.titled, .closable, .miniaturizable, .resizable, .fullSizeContentView]
        
        self.windowDelegate = flags.contains(.main) ? MainDelegate() : SubDelegate()
        super.init(
            contentRect: NSRect(x: 0.0, y: 0.0, width: 960.0, height: 540.0),
            styleMask: styleMask,
            backing: .buffered,
            defer: false
        )
        self.delegate = self.windowDelegate
        self.acceptsMouseMovedEvents = true
        self.titlebarAppearsTransparent = true
        self.titleVisibility = .hidden
        self.title = "Peridot Marble Editor"
        
        self.mainView.setup()
        self.contentView = self.mainView
    }
    
    var metalLayer: CAMetalLayer {
        get {
            return self.mainView.actualLayer
        }
    }
    
    func setCallbacks(
        _ callbacks: UnsafePointer<WindowLinkCallbacks>,
        caller callerContext: UnsafeMutableRawPointer
    ) {
        self.callbacks = CallbackSet(funcs: callbacks, ctx: callerContext, owner: self)
        self.mainView.windowLinkCallbacks = self.callbacks
        self.windowDelegate.callbacks = self.callbacks
    }
    
    func getCallbackContextPointer() -> UnsafeMutableRawPointer? {
        self.callbacks?.getContextPointer()
    }
    
    func unsetCallbacks() {
        self.callbacks = nil
        self.mainView.windowLinkCallbacks = nil
    }
    
    func showAsPrimary() {
        self.center()
        self.makeKeyAndOrderFront(nil)
    }
    
    func show() {
        self.orderFront(nil)
    }
    
    func acceptsKeyInputsToView() {
        self.makeFirstResponder(self.mainView)
    }
    
    func acceptsKeyInputsToWindow() {
        self.makeFirstResponder(nil)
    }
    
    override func mouseDown(with event: NSEvent) {
        let p = event.locationInWindow
        self.callbacks?.notifyPointerDown(Double(p.x), Double(self.frame.height - p.y), MouseButtonLeft)
    }
    
    override func rightMouseDown(with event: NSEvent) {
        let p = event.locationInWindow
        self.callbacks?.notifyPointerDown(Double(p.x), Double(self.frame.height - p.y), MouseButtonRight)
    }
    
    override func mouseMoved(with event: NSEvent) {
        let p = event.locationInWindow
        self.callbacks?.notifyPointerMove(Double(p.x), Double(self.frame.height - p.y))
    }
    
    override func mouseDragged(with event: NSEvent) {
        let p = event.locationInWindow
        self.callbacks?.notifyPointerMove(Double(p.x), Double(self.frame.height - p.y))
    }
    
    override func mouseUp(with event: NSEvent) {
        if event.clickCount == 2 && isOnTitleBar(event.locationInWindow) {
            self.performZoom(self)
            return
        }
        
        self.callbacks?.notifyPointerUp(MouseButtonLeft)
    }
    
    override func rightMouseUp(with event: NSEvent) {
        self.callbacks?.notifyPointerUp(MouseButtonRight)
    }
    
    override func keyDown(with event: NSEvent) {
        if let characters = event.characters {
            if characters.unicodeScalars.count > 1 {
                logger.warning("multiple unicode codepoint contained in keydown event: \(characters)")
            }
        
            self.callbacks?.notifyKeyDownWithChar(event.keyCode, event.modifierFlags, characters.unicodeScalars.first!)
        } else {
            self.callbacks?.notifyKeyDown(event.keyCode, event.modifierFlags)
        }
    }
    
    override func keyUp(with event: NSEvent) {
        self.callbacks?.notifyKeyUp(event.keyCode, event.modifierFlags)
    }
    
    private func isOnTitleBar(_ point: NSPoint) -> Bool {
        // https://stackoverflow.com/a/61712229
        NSRect(
            x: self.contentLayoutRect.minX,
            y: self.contentLayoutRect.minY + self.contentLayoutRect.height,
            width: self.contentLayoutRect.width,
            height: self.frame.height - self.contentLayoutRect.height
        ).contains(point)
    }
    
    struct CreationFlags : OptionSet {
        let rawValue: UInt32
        
        static let main = Self(rawValue: 0x01)
    }
    
    final class CallbackSet {
        private let funcs: UnsafePointer<WindowLinkCallbacks>
        private let ctx: UnsafeMutableRawPointer
        private unowned let owner: WindowLink
        
        init(funcs: UnsafePointer<WindowLinkCallbacks>, ctx: UnsafeMutableRawPointer, owner: WindowLink) {
            self.funcs = funcs
            self.ctx = ctx
            self.owner = owner
        }
        
        deinit {
            self.funcs.pointee.destructor(self.ctx)
        }
        
        private var windowLinkPointer: OpaquePointer {
            get { return OpaquePointer(Unmanaged.passUnretained(self.owner).toOpaque()) }
        }
        
        func getContextPointer() -> UnsafeMutableRawPointer {
            self.ctx
        }
        
        func performCloseAction() {
            self.funcs.pointee.onWindowClose(self.ctx, self.windowLinkPointer)
        }
        
        func notifyKeyFocusStateChanged(_ focused: Bool) {
            self.funcs.pointee.onKeyFocusStateChanged(
                self.ctx,
                self.windowLinkPointer,
                focused ? 1 : 0
            )
        }
        
        func notifyResize(_ width: Double, _ height: Double) {
            self.funcs.pointee.onResize(self.ctx, self.windowLinkPointer, width, height)
        }
        
        func notifyPointerDown(_ x: Double, _ y: Double, _ button: UInt8) {
            self.funcs.pointee.onPointerDown(self.ctx, self.windowLinkPointer, x, y, button)
        }
        
        func notifyPointerMove(_ x: Double, _ y: Double) {
            self.funcs.pointee.onPointerMove(self.ctx, self.windowLinkPointer, x, y)
        }
        
        func notifyPointerUp(_ button: UInt8) {
            self.funcs.pointee.onPointerUp(self.ctx, self.windowLinkPointer, button)
        }
        
        func notifyKeyDown(_ code: UInt16, _ modifierFlags: NSEvent.ModifierFlags) {
            self.funcs.pointee.onKeyDown(
                self.ctx,
                self.windowLinkPointer,
                code,
                UInt32(modifierFlags.rawValue)
            )
        }
        
        func notifyKeyDownWithChar(_ code: UInt16, _ modifierFlags: NSEvent.ModifierFlags, _ ch: Unicode.Scalar) {
            self.funcs.pointee.onKeyDownWithChar(
                self.ctx,
                self.windowLinkPointer,
                code,
                UInt32(modifierFlags.rawValue),
                ch.value
            )
        }
        
        func notifyKeyUp(_ code: UInt16, _ modifierFlags: NSEvent.ModifierFlags) {
            self.funcs.pointee.onKeyUp(
                self.ctx,
                self.windowLinkPointer,
                code,
                UInt32(modifierFlags.rawValue)
            )
        }
    }

    class Delegate : NSObject, NSWindowDelegate {
        unowned var callbacks: CallbackSet? = nil
        
        func windowDidBecomeKey(_ notification: Notification) {
            self.callbacks?.notifyKeyFocusStateChanged(true)
        }
        
        func windowDidResignKey(_ notification: Notification) {
            self.callbacks?.notifyKeyFocusStateChanged(false)
        }
    }

    final class MainDelegate : Delegate {
        func windowWillClose(_ notification: Notification) {
            app.terminate(nil)
        }
    }

    final class SubDelegate : Delegate {
        func windowShouldClose(_ sender: NSWindow) -> Bool {
            self.callbacks?.performCloseAction()
            return true
        }
    }
}

struct TextInputClientForwarding {
    let ftable: UnsafePointer<TextInputClientForwardingFT>
    let context: UnsafeMutableRawPointer
}

final class MainView : NSView, NSTextInputClient {
    var windowLinkCallbacks: WindowLink.CallbackSet? = nil
    var contentsScale: CGFloat = 1.0
    var textInputClientForwarding: TextInputClientForwarding? = nil
    
    func setup() {
        self.wantsLayer = true
        self.layerContentsRedrawPolicy = .never
        self.layerContentsPlacement = .scaleAxesIndependently
        
        let layer = CAMetalLayer()
        layer.framebufferOnly = false // BackdropBlurやるのに必要（ただgrabしない形のやり方も考えた方が良さそう そっちの方が効率いいはず）
        let scaling = self.convertToBacking(NSSize(width: 1.0, height: 1.0))
        self.contentsScale = min(scaling.width, scaling.height)
        layer.contentsScale = self.contentsScale
        self.layer = layer
    }
    
    override var acceptsFirstResponder: Bool {
        get { return true }
    }
    
    var backingSize: CGSize {
        get {
            return self.convertToBacking(self.frame.size)
        }
    }
    
    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        let drawableSize = self.convertToBacking(newSize)
        self.windowLinkCallbacks?.notifyResize(drawableSize.width, drawableSize.height)
    }
    
    var actualLayer: CAMetalLayer {
        get {
            return self.layer! as! CAMetalLayer
        }
    }
    
    override func keyDown(with event: NSEvent) {
        if self.textInputClientForwarding == nil {
            // no text input context available
            super.keyDown(with: event)
            return
        }
        
        self.inputContext!.handleEvent(event)
    }
    
    // NSTextInputClient
    
    func hasMarkedText() -> Bool {
        guard let fw = self.textInputClientForwarding else {
            return false
        }
        
        return fw.ftable.pointee.hasMarkedText(fw.context) != 0
    }
    
    func markedRange() -> NSRange {
        guard let fw = self.textInputClientForwarding else {
            return NSRange(location: NSNotFound, length: 0)
        }
        
        var location: Int64 = 0
        var length: Int64 = 0
        if fw.ftable.pointee.markedRange(fw.context, &location, &length) == 0 {
            return NSRange(location: NSNotFound, length: 0)
        }
        return NSRange(location: Int(location), length: Int(length))
    }
    
    func selectedRange() -> NSRange {
        guard let fw = self.textInputClientForwarding else {
            return NSRange(location: NSNotFound, length: 0)
        }
        
        var location: Int64 = 0
        var length: Int64 = 0
        fw.ftable.pointee.selectedRange(fw.context, &location, &length)
        return NSRange(location: Int(location), length: Int(length))
    }
    
    func setMarkedText(_ string: Any, selectedRange: NSRange, replacementRange: NSRange) {
        let fw = self.textInputClientForwarding!
        
        if let s = string as? NSString {
            fw.ftable.pointee.setMarkedText(
                fw.context,
                s.utf8String,
                Int64(selectedRange.location),
                Int64(selectedRange.length),
                Int64(replacementRange.location),
                Int64(replacementRange.length)
            )
        } else if let s = string as? NSAttributedString {
            s.string.utf8CString.withUnsafeBufferPointer { strptr in
                fw.ftable.pointee.setMarkedText(
                    fw.context,
                    strptr.baseAddress,
                    Int64(selectedRange.location),
                    Int64(selectedRange.length),
                    Int64(replacementRange.location),
                    Int64(replacementRange.length)
                )
            }
        } else {
            logger.error("todo: setMarkedText(invalid call?) \(selectedRange) \(replacementRange)")
        }
    }
    
    func unmarkText() {
        logger.warning("todo: unmarkText")
    }
    
    func validAttributesForMarkedText() -> [NSAttributedString.Key] {
        logger.warning("todo: validAttributesForMarkedText")
        return []
    }
    
    func attributedSubstring(forProposedRange range: NSRange, actualRange: NSRangePointer?) -> NSAttributedString? {
        let fw = self.textInputClientForwarding!
        var subStringPtr: UnsafePointer<CChar>? = nil
        var subStringLength: UInt64 = 0
        
        if let actualRange = actualRange {
            var actualLocation: Int64 = 0
            var actualLength: Int64 = 0
            
            fw.ftable.pointee.substring(
                fw.context,
                range.location == NSNotFound ? 1 : 0,
                Int64(range.location),
                Int64(range.length),
                &actualLocation,
                &actualLength,
                &subStringPtr,
                &subStringLength
            )
            
            actualRange.pointee.location = Int(actualLocation)
            actualRange.pointee.length = Int(actualLength)
        } else {
            fw.ftable.pointee.substring(
                fw.context,
                range.location == NSNotFound ? 1 : 0,
                Int64(range.location),
                Int64(range.length),
                nil,
                nil,
                &subStringPtr,
                &subStringLength
            )
        }
        
        let str = subStringPtr!.withMemoryRebound(to: UInt8.self, capacity: Int(subStringLength)) { castedPtr in
            String(bytes: UnsafeBufferPointer(start: castedPtr, count: Int(subStringLength)), encoding: .utf8)!
        }
        return NSAttributedString(string: str)
    }
    
    func insertText(_ string: Any, replacementRange: NSRange) {
        logger.log("insertText")
        let fw = self.textInputClientForwarding!
        
        if let s = string as? NSString {
            fw.ftable.pointee.insertText(
                fw.context,
                s.utf8String,
                Int64(replacementRange.location),
                Int64(replacementRange.length),
            )
        } else if let s = string as? NSAttributedString {
            s.string.utf8CString.withUnsafeBufferPointer { strptr in
                fw.ftable.pointee.insertText(
                    fw.context,
                    strptr.baseAddress,
                    Int64(replacementRange.location),
                    Int64(replacementRange.length),
                )
            }
        } else {
            logger.warning("todo: insertText(invalid call?) \(replacementRange)")
        }
    }
    
    func characterIndex(for point: NSPoint) -> Int {
        logger.warning("todo: characterIndex")
        return 0
    }
    
    func firstRect(forCharacterRange range: NSRange, actualRange: NSRangePointer?) -> NSRect {
        guard let fw = self.textInputClientForwarding else {
            return NSRect()
        }
        
        var x: CFloat = 0.0
        var y: CFloat = 0.0
        var width: CFloat = 0.0
        var height: CFloat = 0.0
        
        if let actualRange = actualRange {
            var actualLocation: Int64 = 0
            var actualLength: Int64 = 0
            
            fw.ftable.pointee.firstRect(
                fw.context,
                Int64(range.location),
                Int64(range.length),
                &actualLocation,
                &actualLength,
                &x,
                &y,
                &width,
                &height,
            );
            
            actualRange.pointee.location = Int(actualLocation)
            actualRange.pointee.length = Int(actualLength)
        } else {
            fw.ftable.pointee.firstRect(
                fw.context,
                Int64(range.location),
                Int64(range.length),
                nil,
                nil,
                &x,
                &y,
                &width,
                &height,
            );
        }
        
        return self.window!.convertToScreen(NSRect(
            x: CGFloat(x),
            // bottomである必要がある
            y: self.window!.frame.height - CGFloat(y + height),
            width: CGFloat(width),
            height: CGFloat(height)
        ))
    }
    
    override func doCommand(by selector: Selector) {
        if !self.tryToPerform(selector, with: self) {
            logger.warning("doCommand fail: \(selector)")
        }
    }
    
    override func moveRight(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, NSEvent.ModifierFlags(), Unicode.Scalar(NSRightArrowFunctionKey)!)
    }
    
    override func moveLeft(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, NSEvent.ModifierFlags(), Unicode.Scalar(NSLeftArrowFunctionKey)!)
    }
    
    override func scrollToBeginningOfDocument(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, NSEvent.ModifierFlags(), Unicode.Scalar(NSHomeFunctionKey)!)
    }
    
    override func scrollToEndOfDocument(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, NSEvent.ModifierFlags(), Unicode.Scalar(NSEndFunctionKey)!)
    }
    
    override func deleteBackward(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, NSEvent.ModifierFlags(), Unicode.Scalar(NSBackspaceCharacter)!)
    }
    
    override func deleteForward(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, NSEvent.ModifierFlags(), Unicode.Scalar(NSDeleteFunctionKey)!)
    }
    
    override func moveRightAndModifySelection(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, .shift, Unicode.Scalar(NSRightArrowFunctionKey)!)
    }
    
    override func moveLeftAndModifySelection(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, .shift, Unicode.Scalar(NSLeftArrowFunctionKey)!)
    }
}

@_cdecl("ni_create_window")
func createWindow(flags: UInt32) -> UnsafeMutableRawPointer {
    return Unmanaged.passRetained(WindowLink(WindowLink.CreationFlags(rawValue: flags))).toOpaque()
}

@_cdecl("ni_release_window")
func releaseWindow(p: UnsafeMutableRawPointer) {
    // Note: deallocしてはいけないらしい（閉じる時のアニメーションしてる間生きてる必要がある？）
    Unmanaged<WindowLink>.fromOpaque(p).takeUnretainedValue().close()
}

@_cdecl("ni_show_window")
func showWindow(windowLink: UnsafeMutableRawPointer) {
    Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().show()
}

@_cdecl("ni_show_window_as_primary")
func showWindowAsPrimary(windowLink: UnsafeMutableRawPointer) {
    Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().showAsPrimary()
}

@_cdecl("ni_get_content_scale")
func getContentScale(windowLink: UnsafeMutableRawPointer) -> Float {
    Float(Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().mainView.contentsScale)
}

@_cdecl("ni_set_window_callbacks")
func setWindowCallbacks(
    windowLink: UnsafeMutableRawPointer,
    callbacks: UnsafePointer<WindowLinkCallbacks>,
    callerContext: UnsafeMutableRawPointer
) {
    Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().setCallbacks(callbacks, caller: callerContext)
}

@_cdecl("ni_get_window_callback_context")
func getWindowCallbackContext(windowLink: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer? {
    Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().getCallbackContextPointer()
}

@_cdecl("ni_accepts_key_inputs_to_view")
func acceptsKeyInputsToView(
    _ windowLink: UnsafeMutableRawPointer,
    _ textInputClientForwardingFT: UnsafePointer<TextInputClientForwardingFT>,
    _ textInputClientForwardingContext: UnsafeMutableRawPointer
) {
    let w = Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue()
    
    w.mainView.textInputClientForwarding = TextInputClientForwarding(
        ftable: textInputClientForwardingFT,
        context: textInputClientForwardingContext
    )
    w.acceptsKeyInputsToView()
}

@_cdecl("ni_accepts_key_inputs_to_window")
func acceptsKeyInputsToWindow(
    _ windowLink: UnsafeMutableRawPointer,
    _ retTextInputClientForwardingFT: UnsafeMutablePointer<UnsafePointer<TextInputClientForwardingFT>>,
    _ retTextInputClientForwardingContext: UnsafeMutablePointer<UnsafeMutableRawPointer>,
) {
    let w = Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue()
    
    w.acceptsKeyInputsToWindow()
    retTextInputClientForwardingFT.pointee = w.mainView.textInputClientForwarding!.ftable
    retTextInputClientForwardingContext.pointee = w.mainView.textInputClientForwarding!.context
    w.mainView.textInputClientForwarding = nil
}

@_cdecl("ni_get_size_logical")
func getSizePixels(windowLink: UnsafeMutableRawPointer, width: UnsafeMutablePointer<CDouble>, height: UnsafeMutablePointer<CDouble>) {
    let size = Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().mainView.backingSize
    width.pointee = size.width
    height.pointee = size.height
}

@_cdecl("ni_get_metal_layer")
func getMetalLayer(windowLink: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer {
    unsafeBitCast(
        Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().metalLayer,
        to: UnsafeMutableRawPointer.self
    )
}

@_cdecl("ni_convert_point_to_screen")
func convertPointToScreen(windowLink: UnsafeMutableRawPointer, x: UnsafeMutablePointer<Double>, y: UnsafeMutablePointer<Double>) {
    let w = Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue()
    // yはtopの値がくる convertPoint:toScreen:に渡すのはbottomである必要があるので変換する
    let result = w.convertPoint(toScreen: NSPoint(x: x.pointee, y: w.frame.height - y.pointee))
    
    x.pointee = result.x
    // こっちは再変換しなくていい（よくわからん）
    y.pointee = result.y
}
