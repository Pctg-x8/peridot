import Cocoa
import os

let app = NSApplication.shared
let delegate = AppMainDelegate()
app.delegate = delegate

let logger = Logger()

let filesystemCacheDir = URL.cachesDirectory.path.utf8CString

let menu = NSMenu()
let appMenu = NSMenu(title: "Peridot Marble Editor")
let quitMenuItem = appMenu.addItem(withTitle: "Quit", action: #selector(NSApplication.terminate), keyEquivalent: "q")
quitMenuItem.keyEquivalentModifierMask = .command
let appMenuItem = menu.addItem(withTitle: "Peridot Marble Editor", action: nil, keyEquivalent: "")
appMenuItem.title = "Peridot Marble Editor"
menu.setSubmenu(appMenu, for: appMenuItem)
app.mainMenu = menu

let dragPreviewWindow = NSPanel(
    contentRect: NSRect(x: 0, y: 0, width: 128, height: 128),
    styleMask: [.nonactivatingPanel, .borderless, .utilityWindow],
    backing: .buffered,
    defer: false
)
dragPreviewWindow.backgroundColor = NSColor(red: 0.0625, green: 0.6875, blue: 1.0, alpha: 0.0625)
let dragPreviewWindowView = NSVisualEffectView()
dragPreviewWindowView.blendingMode = .behindWindow
dragPreviewWindowView.material = .popover
dragPreviewWindowView.state = .active
dragPreviewWindow.contentView = dragPreviewWindowView

rs_launch()

@_cdecl("nsapp_run")
func nsAppRun() {
    app.run()
}

@_cdecl("manual_capture_begin")
func manualCaptureBegin(windowLink: UnsafeMutableRawPointer) {
    let w = Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue()
    
    let mgr = MTLCaptureManager.shared()
    let cd = MTLCaptureDescriptor()
    cd.captureObject = w.mainView.actualLayer.device
    try! mgr.startCapture(with: cd)
}

@_cdecl("manual_capture_end")
func manualCaptureEnd() {
    MTLCaptureManager.shared().stopCapture()
}

final class AppMainDelegate : NSObject, NSApplicationDelegate {}

class AppWindowDelegate : NSObject, NSWindowDelegate {
    unowned var callbacks: WindowLinkCallbackSet? = nil
    
    func windowDidBecomeKey(_ notification: Notification) {
        self.callbacks?.notifyKeyFocusStateChanged(true)
    }
    
    func windowDidResignKey(_ notification: Notification) {
        self.callbacks?.notifyKeyFocusStateChanged(false)
    }
}

final class MainWindowDelegate : AppWindowDelegate {
    func windowWillClose(_ notification: Notification) {
        app.terminate(nil)
    }
}

final class SubWindowDelegate : AppWindowDelegate {
    func windowShouldClose(_ sender: NSWindow) -> Bool {
        self.callbacks?.performCloseAction()
        return true
    }
}

final class WindowLinkCallbackSet {
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

struct WindowCreationFlags : OptionSet {
    let rawValue: UInt32
    
    static let main = Self(rawValue: 0x01)
}

final class WindowLink : NSWindow {
    private let windowDelegate: AppWindowDelegate
    private var callbacks: WindowLinkCallbackSet? = nil
    let mainView = MainView()
    
    init(_ flags: WindowCreationFlags) {
        let styleMask: StyleMask = [.titled, .closable, .miniaturizable, .resizable, .fullSizeContentView]
        
        self.windowDelegate = flags.contains(.main) ? MainWindowDelegate() : SubWindowDelegate()
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
        
        if flags.contains(.main) {
            self.center()
            self.makeKeyAndOrderFront(nil)
        }
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
        self.callbacks = WindowLinkCallbackSet(funcs: callbacks, ctx: callerContext, owner: self)
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
}

final class ContextMenuSurface : NSPanel, NSWindowDelegate {
    let instanceVars: UnsafeMutableRawPointer
    let callbacks: UnsafeMutablePointer<ContextMenuSurfaceCallbacks>
    let mainView: ContextMenuView
    unowned let parentLink: WindowLink
    
    init(
        _ parent: WindowLink,
        _ surfacePos: NSPoint,
        _ instanceVars: UnsafeMutableRawPointer,
        _ callbacks: UnsafeMutablePointer<ContextMenuSurfaceCallbacks>
    ) {
        let screenPos = parent.convertPoint(toScreen: NSPoint(x: surfacePos.x, y: parent.frame.height - surfacePos.y))
        
        self.instanceVars = instanceVars
        self.callbacks = callbacks
        self.mainView = ContextMenuView()
        self.parentLink = parent
        super.init(
            contentRect: NSRect(x: screenPos.x, y: screenPos.y, width: 128.0, height: 128.0),
            styleMask: [.nonactivatingPanel, .borderless, .fullSizeContentView],
            backing: .buffered,
            defer: false
        )
        self.isOpaque = false
        self.backgroundColor = NSColor.clear
        self.delegate = self
        self.acceptsMouseMovedEvents = true
        self.level = .popUpMenu
        self.hasShadow = false
        
        self.mainView.setup()
        let vfxView = NSVisualEffectView()
        vfxView.autoresizingMask = [.width, .height]
        vfxView.blendingMode = .behindWindow
        vfxView.material = .menu
        vfxView.state = .active
        let baseView = NSView()
        baseView.autoresizingMask = [.width, .height]
        baseView.addSubview(vfxView)
        baseView.addSubview(self.mainView)
        self.contentView = baseView
        
        parent.addChildWindow(self, ordered: .above)
    }
    
    func resize(_ size: NSSize) {
        self.setContentSize(size)
    }
    
    var contentsScale: CGFloat {
        get {
            return self.mainView.contentsScale
        }
    }
    var metalLayer: CAMetalLayer {
        get {
            return self.mainView.actualLayer
        }
    }
    
    override func mouseExited(with event: NSEvent) {
        self.callbacks.pointee.onPointerLeave(OpaquePointer(Unmanaged.passUnretained(self).toOpaque()))
    }
    
    override func mouseMoved(with event: NSEvent) {
        let p = event.locationInWindow
        
        self.callbacks.pointee.onPointerMove(
            OpaquePointer(Unmanaged.passUnretained(self).toOpaque()),
            Double(p.x),
            Double(self.frame.height - p.y),
        )
    }
    
    override func mouseDown(with event: NSEvent) {
        let p = event.locationInWindow
        
        self.callbacks.pointee.onPointerDown(
            OpaquePointer(Unmanaged.passUnretained(self).toOpaque()),
            Double(p.x),
            Double(self.frame.height - p.y),
            MouseButtonLeft,
        )
    }
    
    override func rightMouseDown(with event: NSEvent) {
        let p = event.locationInWindow
        
        self.callbacks.pointee.onPointerDown(
            OpaquePointer(Unmanaged.passUnretained(self).toOpaque()),
            Double(p.x),
            Double(self.frame.height - p.y),
            MouseButtonRight,
        )
    }
    
    override func mouseUp(with event: NSEvent) {
        self.callbacks.pointee.onPointerUp(OpaquePointer(Unmanaged.passUnretained(self).toOpaque()), MouseButtonLeft)
    }
    
    override func rightMouseUp(with event: NSEvent) {
        self.callbacks.pointee.onPointerUp(OpaquePointer(Unmanaged.passUnretained(self).toOpaque()), MouseButtonRight)
    }
    
    func windowDidBecomeKey(_ notification: Notification) {
        logger.debug("contextMenuDidBecomeKey")
    }
    
    func windowDidResignKey(_ notification: Notification) {
        logger.debug("contextMenuDidResignKey")
    }
}

@_cdecl("ni_create_window")
func createWindow(flags: UInt32) -> UnsafeMutableRawPointer {
    return Unmanaged.passRetained(WindowLink(WindowCreationFlags(rawValue: flags))).toOpaque()
}

@_cdecl("ni_release_window")
func releaseWindow(p: UnsafeMutableRawPointer) {
    Unmanaged<WindowLink>.fromOpaque(p).takeUnretainedValue().close()
}

@_cdecl("ni_show_window")
func showWindow(windowLink: UnsafeMutableRawPointer) {
    Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().show()
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

@_cdecl("ni_post_unbound_callback_from_thread")
func postUnboundCallbackFromThread(cb: UnboundCallback, ctx: UnsafeMutableRawPointer) {
    DispatchQueue.main.async {
        cb(ctx)
    }
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

@_cdecl("ni_show_drag_preview")
func showDragPreview(x: Double, y: Double, width: Double, height: Double) {
    // top leftの座標が来るのでbottom leftに変換する
    dragPreviewWindow.setFrame(NSRect(x: x, y: y - height, width: width, height: height), display: false)
    dragPreviewWindow.orderFront(dragPreviewWindow)
}

@_cdecl("ni_hide_drag_preview")
func hideDragPreview() {
    dragPreviewWindow.orderOut(dragPreviewWindow)
}

@_cdecl("ni_move_drag_preview")
func moveDragPreview(x: Double, y: Double) {
    dragPreviewWindow.setFrameTopLeftPoint(NSPoint(x: x, y: y))
}

enum CursorShape {
    static let Arrow: UInt8 = 0
    static let Pointer: UInt8 = 1
    static let IBeam: UInt8 = 2
    static let ResizeHorizontal: UInt8 = 3
}

@_cdecl("ni_set_cursor_shape")
func setCursorShape(shape: UInt8) {
    switch (shape) {
    case CursorShape.Arrow:
        NSCursor.arrow.set()
    case CursorShape.Pointer:
        NSCursor.pointingHand.set()
    case CursorShape.IBeam:
        NSCursor.iBeam.set()
    case CursorShape.ResizeHorizontal:
        NSCursor.rowResize.set()
    default:
        NSLog("[PeridotMarbleEditor:Warn] invalid CursorShape value: \(shape)")
        break
    }
}

@_cdecl("ni_query_filesystem_cachedir_path")
func queryFilesystemCachedirPath() -> UnsafePointer<CChar> {
    filesystemCacheDir.withUnsafeBufferPointer { $0.baseAddress! }
}

final class ThreadPriorityContext {
    let threadPriority: Double
    
    init(threadPriority: Double) {
        self.threadPriority = threadPriority
    }
}

@_cdecl("ni_degreade_thread_priroity_temporarily")
func degradeThreadPriorityTemporarily() -> UnsafeMutableRawPointer {
    let context = Unmanaged.passRetained(ThreadPriorityContext(threadPriority: Thread.current.threadPriority))
    Thread.current.threadPriority = 0.5
    return context.toOpaque()
}

@_cdecl("ni_restore_thread_priority")
func restoreThreadPriority(contextPtr: UnsafeMutableRawPointer) {
    let context = Unmanaged<ThreadPriorityContext>.fromOpaque(contextPtr).takeUnretainedValue()
    Thread.current.threadPriority = context.threadPriority
    Unmanaged<ThreadPriorityContext>.fromOpaque(contextPtr).release()
}

var currentHoveringTimeoutWorkItem: DispatchWorkItem? = nil

@_cdecl("ni_set_pointer_hovering_timeout")
func setPointerHoveringTimeout(_ millis: UInt32) {
    let workItem = DispatchWorkItem {
        NSLog("Pointer Hovering timeout!")
    }
    currentHoveringTimeoutWorkItem = workItem
    DispatchQueue.main.asyncAfter(
        deadline: DispatchTime.now().advanced(by: .milliseconds(Int(millis))),
        execute: workItem
    )
}

@_cdecl("ni_kill_pointer_hovering_timeout")
func killPointerHoveringTimeout() {
    currentHoveringTimeoutWorkItem?.cancel()
    currentHoveringTimeoutWorkItem = nil
}

@_cdecl("ni_create_context_menu_surface")
func createContextMenuSurface(
    _ parent: UnsafeMutableRawPointer,
    _ x: CFloat,
    _ y: CFloat,
    _ width: CFloat,
    _ height: CFloat,
    _ instanceVars: UnsafeMutableRawPointer,
    _ callbacks: UnsafeMutablePointer<ContextMenuSurfaceCallbacks>,
) -> UnsafeMutableRawPointer {
    Unmanaged.passRetained(ContextMenuSurface(
        Unmanaged<WindowLink>.fromOpaque(parent).takeUnretainedValue(),
        NSPoint(x: CGFloat(x), y: CGFloat(y)),
        instanceVars,
        callbacks,
    )).toOpaque()
}

@_cdecl("ni_release_context_menu_surface")
func releaseContextMenuSurface(
    _ surface: UnsafeMutableRawPointer,
    _ retInstanceVars: UnsafeMutablePointer<UnsafeMutableRawPointer>,
    _ retCallbacks: UnsafeMutablePointer<UnsafeMutablePointer<ContextMenuSurfaceCallbacks>>
) {
    let surface = Unmanaged<ContextMenuSurface>.fromOpaque(surface).takeRetainedValue()
    retInstanceVars.pointee = surface.instanceVars
    retCallbacks.pointee = surface.callbacks
    surface.close()
}

@_cdecl("ni_context_menu_get_metal_layer")
func getContextMenuMetalLayer(_ surface: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer {
    unsafeBitCast(
        Unmanaged<ContextMenuSurface>.fromOpaque(surface).takeUnretainedValue().metalLayer,
        to: UnsafeMutableRawPointer.self
    )
}

@_cdecl("ni_context_menu_get_content_scale")
func getContextMenuContentScale(_ surface: UnsafeMutableRawPointer) -> Float {
    Float(Unmanaged<ContextMenuSurface>.fromOpaque(surface).takeUnretainedValue().contentsScale)
}

@_cdecl("ni_context_menu_resize")
func contextMenuResize(_ surface: UnsafeMutableRawPointer, _ width: CFloat, _ height: CFloat) {
    Unmanaged<ContextMenuSurface>.fromOpaque(surface).takeUnretainedValue().resize(NSSize(width: CGFloat(width), height: CGFloat(height)))
}

@_cdecl("ni_context_menu_instance_vars_ptr")
func contextMenuGetInstanceVarsPtr(_ surface: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer {
    Unmanaged<ContextMenuSurface>.fromOpaque(surface).takeUnretainedValue().instanceVars
}

var contextMenuReservedDelayedActionWorker: DispatchWorkItem? = nil

@_cdecl("ni_context_menu_reserve_delayed_action")
func contextMenuReserveDelayedAction(_ delayMilliseconds: CInt, _ callback: UnboundCallback, _ ctx: UnsafeMutableRawPointer) {
    if let oldWork = contextMenuReservedDelayedActionWorker {
        // remove old
        oldWork.cancel()
    }
    
    contextMenuReservedDelayedActionWorker = DispatchWorkItem {
        callback(ctx)
    }
    DispatchQueue.main.asyncAfter(
        deadline: DispatchTime.now().advanced(by: DispatchTimeInterval.milliseconds(Int(delayMilliseconds))),
        execute: contextMenuReservedDelayedActionWorker!
    )
}

@_cdecl("ni_context_menu_unreserve_delayed_action")
func contextMenuUnreserveDelayedAction() {
    contextMenuReservedDelayedActionWorker?.cancel()
    contextMenuReservedDelayedActionWorker = nil
}

var contextMenuGlobalMonitor: Any? = nil
var contextMenuLocalMonitor: Any? = nil

@_cdecl("ni_context_menu_observe_global_click")
func contextMenuObserveGlobalClick(_ callback: ContextMenuGlobalClickCallback, _ ctx: UnsafeMutableRawPointer) {
    contextMenuGlobalMonitor = NSEvent.addGlobalMonitorForEvents(
        matching: [.leftMouseDown, .leftMouseUp, .rightMouseDown, .rightMouseUp, .otherMouseDown, .otherMouseUp]
    ) { event in
        callback(ctx, event.window is ContextMenuSurface ? 1 : 0)
    }
    contextMenuLocalMonitor = NSEvent.addLocalMonitorForEvents(
        matching: [.leftMouseDown, .leftMouseUp, .rightMouseDown, .rightMouseUp, .otherMouseDown, .otherMouseUp]
    ) { event in
        callback(ctx, event.window is ContextMenuSurface ? 1 : 0)
        return event
    }
}

@_cdecl("ni_context_menu_unobserve_global_click")
func contextMenuUnobserveGlobalClick() {
    if let m = contextMenuGlobalMonitor {
        NSEvent.removeMonitor(m)
        contextMenuGlobalMonitor = nil
    }
    
    if let m = contextMenuLocalMonitor {
        NSEvent.removeMonitor(m)
        contextMenuLocalMonitor = nil
    }
}

enum CustomAttributeKey: NSString {
    case SpacingInlineStart = "peridot.spacing_inline_start"
    case FontID = "peridot.font_id"
}

@_cdecl("ni_ak_spacing_inline_start")
func akSpacingInlineStart() -> CFString {
    CustomAttributeKey.SpacingInlineStart.rawValue
}

@_cdecl("ni_ak_font_id")
func akFontID() -> CFString {
    CustomAttributeKey.FontID.rawValue
}

// TODO: できればNoCopyにしたい StringのNoCopy系initがdeprecatedなので使うのはちょっと厳しい......
@_cdecl("ni_log_err")
func logErr(_ charbuf: UnsafePointer<UInt8>) {
    logger.error("\(String(cString: charbuf))")
}
@_cdecl("ni_log_warn")
func logWarn(_ charbuf: UnsafePointer<UInt8>) {
    logger.warning("\(String(cString: charbuf))")
}
@_cdecl("ni_log_info")
func logInfo(_ charbuf: UnsafePointer<UInt8>) {
    logger.info("\(String(cString: charbuf))")
}
@_cdecl("ni_log_debug")
func logDebug(_ charbuf: UnsafePointer<UInt8>) {
    logger.debug("\(String(cString: charbuf))")
}
@_cdecl("ni_log_trace")
func logTrace(_ charbuf: UnsafePointer<UInt8>) {
    logger.trace("\(String(cString: charbuf))")
}
@_cdecl("ni_log_fault")
func logFault(_ charbuf: UnsafePointer<UInt8>) {
    logger.fault("\(String(cString: charbuf))")
}
