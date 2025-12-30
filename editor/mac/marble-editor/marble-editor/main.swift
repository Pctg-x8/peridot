import Cocoa

let app = NSApplication.shared
let delegate = AppMainDelegate()
app.delegate = delegate

let w = NSWindow(
    contentRect: NSRect(
        origin: NSPoint(x: 0.0, y: 0.0),
        size: NSSize(width: 960.0, height: 540.0)
    ),
    styleMask: [.titled, .closable, .miniaturizable, .resizable],
    backing: .buffered,
    defer: false
)
w.title = "Peridot Marble Editor"
w.center()

let mainView = MainView()
w.contentView = mainView
mainView.setup()

w.makeKeyAndOrderFront(nil)
rs_launch()

@_cdecl("nsapp_run")
func nsAppRun() {
    NSApplication.shared.run()
}

@_cdecl("get_main_metal_layer")
func getMainMetalLayer() -> UnsafeMutableRawPointer {
    return unsafeBitCast(mainView.actualLayer, to: UnsafeMutableRawPointer.self)
}

@_cdecl("manual_capture_begin")
func manualCaptureBegin() {
    let mgr = MTLCaptureManager.shared()
    let cd = MTLCaptureDescriptor()
    cd.captureObject = mainView.actualLayer.device
    try! mgr.startCapture(with: cd)
}

@_cdecl("manual_capture_end")
func manualCaptureEnd() {
    MTLCaptureManager.shared().stopCapture()
}

final class AppMainDelegate : NSObject, NSApplicationDelegate {}
