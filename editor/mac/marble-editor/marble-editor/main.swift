import Cocoa

let app = NSApplication.shared
let delegate = AppMainDelegate()
app.delegate = delegate

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

protocol AppWindowDelegate : NSWindowDelegate {}

final class MainWindowDelegate : NSObject, AppWindowDelegate {
    func windowWillClose(_ notification: Notification) {
        app.terminate(nil)
    }
}

final class SubWindowDelegate : NSObject, AppWindowDelegate {
    unowned var callbacks: WindowLinkCallbackSet? = nil
    
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
    
    func getContextPointer() -> UnsafeMutableRawPointer {
        self.ctx
    }
    
    func performCloseAction() {
        self.funcs.pointee.onWindowClose(self.ctx, OpaquePointer(Unmanaged.passUnretained(self.owner).toOpaque()))
    }
    
    func notifyResize(_ width: Double, _ height: Double) {
        self.funcs.pointee.onResize(self.ctx, OpaquePointer(Unmanaged.passUnretained(self.owner).toOpaque()), width, height)
    }
    
    func notifyPointerDown(_ x: Double, _ y: Double) {
        self.funcs.pointee.onPointerDown(self.ctx, OpaquePointer(Unmanaged.passUnretained(self.owner).toOpaque()), x, y)
    }
    
    func notifyPointerMove(_ x: Double, _ y: Double) {
        self.funcs.pointee.onPointerMove(self.ctx, OpaquePointer(Unmanaged.passUnretained(self.owner).toOpaque()), x, y)
    }
    
    func notifyPointerUp() {
        self.funcs.pointee.onPointerUp(self.ctx, OpaquePointer(Unmanaged.passUnretained(self.owner).toOpaque()))
    }
}

struct WindowCreationFlags : OptionSet {
    let rawValue: UInt32
    
    static let main = Self(rawValue: 0x01)
}

final class WindowLink : NSWindow {
    private var windowDelegate: AppWindowDelegate? = nil
    private var callbacks: WindowLinkCallbackSet? = nil
    
    init(_ flags: WindowCreationFlags) {
        var styleMask: StyleMask = [.titled, .closable, .miniaturizable, .resizable, .fullSizeContentView]
        
        super.init(
            contentRect: NSRect(x: 0.0, y: 0.0, width: 960.0, height: 540.0),
            styleMask: styleMask,
            backing: .buffered,
            defer: false
        )
        self.acceptsMouseMovedEvents = true
        self.titlebarAppearsTransparent = true
        self.titleVisibility = .hidden
        self.title = "Peridot Marble Editor"
        
        let mainView = MainView()
        mainView.setup()
        self.contentView = mainView
        
        if !flags.contains(.main) {
            self.windowDelegate = SubWindowDelegate()
            self.delegate = self.windowDelegate
        }
    }
    
    var mainView: MainView {
        get {
            return self.contentView! as! MainView
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
        if let subWindowDelegate = self.delegate as? SubWindowDelegate {
            subWindowDelegate.callbacks = self.callbacks
        }
    }
    
    func getCallbackContextPointer() -> UnsafeMutableRawPointer? {
        self.callbacks?.getContextPointer()
    }
    
    func unsetCallbacks() {
        self.callbacks = nil
        self.mainView.windowLinkCallbacks = nil
    }
    
    func makePrimaryWindow() {
        self.windowDelegate = MainWindowDelegate()
        self.delegate = self.windowDelegate
        self.center()
        self.makeKeyAndOrderFront(nil)
    }
    
    func show() {
        self.orderFront(nil)
    }
    
    override func mouseDown(with event: NSEvent) {
        let p = event.locationInWindow
        self.callbacks?.notifyPointerDown(Double(p.x), Double(self.frame.height - p.y))
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
        self.callbacks?.notifyPointerUp()
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

@_cdecl("ni_make_primary_window")
func makePrimaryWindow(windowLink: UnsafeMutableRawPointer) {
    Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().makePrimaryWindow()
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
