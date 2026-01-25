import Cocoa

let app = NSApplication.shared
let delegate = AppMainDelegate()
app.delegate = delegate

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
    styleMask: [.nonactivatingPanel, .borderless],
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

final class MainWindowDelegate : NSObject, NSWindowDelegate {
    func windowWillClose(_ notification: Notification) {
        app.terminate(nil)
    }
}

struct WindowLinkCallbackSet {
    private let funcs: UnsafePointer<WindowLinkCallbacks>
    private let ctx: UnsafeMutableRawPointer
    
    init(funcs: UnsafePointer<WindowLinkCallbacks>, ctx: UnsafeMutableRawPointer) {
        self.funcs = funcs
        self.ctx = ctx
    }
    
    func notifyResize(_ width: UInt32, _ height: UInt32) {
        self.funcs.pointee.onResize(self.ctx, width, height)
    }
    
    func notifyPointerDown(_ x: Double, _ y: Double) {
        self.funcs.pointee.onPointerDown(self.ctx, x, y)
    }
    
    func notifyPointerUp() {
        self.funcs.pointee.onPointerUp(self.ctx)
    }
}

final class WindowLink {
    let w: NSWindow
    private var mainWindowDelegate: MainWindowDelegate? = nil
    private var callbacks: WindowLinkCallbackSet? = nil
    
    init(_ w: NSWindow) {
        self.w = w
        
        let mainView = MainView()
        mainView.setup()
        self.w.contentView = mainView
    }
    
    var mainView: MainView {
        get {
            return self.w.contentView! as! MainView
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
        self.callbacks = WindowLinkCallbackSet(funcs: callbacks, ctx: callerContext)
        self.mainView.windowLinkCallbacks = self.callbacks
    }
    
    func unsetCallbacks() {
        self.callbacks = nil
        self.mainView.windowLinkCallbacks = nil
    }
    
    func makePrimaryWindow() {
        self.mainWindowDelegate = MainWindowDelegate()
        self.w.delegate = self.mainWindowDelegate
        self.w.center()
        self.w.makeKeyAndOrderFront(nil)
    }
}

@_cdecl("ni_create_window")
func createWindow() -> UnsafeMutableRawPointer {
    let w = NSWindow(
        contentRect: NSRect(x: 0.0, y: 0.0, width: 960.0, height: 540.0),
        styleMask: [.titled, .closable, .miniaturizable, .resizable],
        backing: .buffered,
        defer: false
    )
    w.title = "Peridot Marble Editor"
    
    return Unmanaged.passRetained(WindowLink(w)).toOpaque()
}

@_cdecl("ni_release_window")
func releaseWindow(p: UnsafeMutableRawPointer) {
    Unmanaged<WindowLink>.fromOpaque(p).release()
}

@_cdecl("ni_make_primary_window")
func makePrimaryWindow(windowLink: UnsafeMutableRawPointer) {
    Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().makePrimaryWindow()
}

@_cdecl("ni_set_window_callbacks")
func setWindowCallbacks(
    windowLink: UnsafeMutableRawPointer,
    callbacks: UnsafePointer<WindowLinkCallbacks>,
    callerContext: UnsafeMutableRawPointer
) {
    Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().setCallbacks(callbacks, caller: callerContext)
}

@_cdecl("ni_unset_window_callbacks")
func unsetWindowCallbacks(windowLink: UnsafeMutableRawPointer) {
    Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().unsetCallbacks()
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
    let result = Unmanaged<WindowLink>.fromOpaque(windowLink).takeUnretainedValue().w
        .convertPoint(toScreen: NSPoint(x: x.pointee, y: y.pointee))
    x.pointee = result.x
    y.pointee = result.y
}

@_cdecl("ni_show_drag_preview")
func showDragPreview() {
    dragPreviewWindow.orderFront(dragPreviewWindow)
}

@_cdecl("ni_hide_drag_preview")
func hideDragPreview() {
    dragPreviewWindow.orderOut(dragPreviewWindow)
}

@_cdecl("ni_move_drag_preview")
func moveDragPreview(x: Double, y: Double, width: Double, height: Double) {
    dragPreviewWindow.setFrame(NSRect(x: x, y: y, width: width, height: height), display: true)
}
