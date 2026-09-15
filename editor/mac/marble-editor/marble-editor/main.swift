import Cocoa
import os
import NaturalLanguage

let app = NSApplication.shared
let delegate = AppMainDelegate()
app.delegate = delegate

let logger = Logger()

let filesystemCacheDir = URL.cachesDirectory.path.utf8CString
let filesystemPersistStateDir = URL.applicationSupportDirectory.path.utf8CString

let menu = NSMenu()
let appMenu = NSMenu(title: "Peridot Marble Editor")
let quitMenuItem = appMenu.addItem(withTitle: "Quit", action: #selector(NSApplication.terminate), keyEquivalent: "q")
quitMenuItem.keyEquivalentModifierMask = .command
let appMenuItem = menu.addItem(withTitle: "Peridot Marble Editor", action: nil, keyEquivalent: "")
appMenuItem.title = "Peridot Marble Editor"
menu.setSubmenu(appMenu, for: appMenuItem)
app.mainMenu = menu

rs_launch()

@_cdecl("nsapp_run")
func nsAppRun() {
    app.run()
}

final class AppMainDelegate : NSObject, NSApplicationDelegate {}

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

@_cdecl("ni_post_unbound_callback_from_thread")
func postUnboundCallbackFromThread(cb: UnboundCallback, ctx: UnsafeMutableRawPointer) {
    DispatchQueue.main.async {
        cb(ctx)
    }
}

enum CursorShape {
    static let Arrow: UInt8 = 0
    static let Pointer: UInt8 = 1
    static let IBeam: UInt8 = 2
    static let ResizeHorizontal: UInt8 = 3
    static let ResizeVertical: UInt8 = 4
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
        NSCursor.columnResize.set()
    case CursorShape.ResizeVertical:
        NSCursor.rowResize.set()
    default:
        logger.warning("invalid CursorShape value: \(shape)")
        break
    }
}

@_cdecl("ni_query_filesystem_cachedir_path")
func queryFilesystemCachedirPath() -> UnsafePointer<CChar> {
    filesystemCacheDir.withUnsafeBufferPointer { $0.baseAddress! }
}

@_cdecl("ni_query_filesystem_persist_statedir_path")
func queryFilesystemPersistStateDirPath() -> UnsafePointer<CChar> {
    filesystemPersistStateDir.withUnsafeBufferPointer { $0.baseAddress! }
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

@_cdecl("ni_query_range_for_word_at")
func queryRangeForWordAt(
    _ charptr: UnsafePointer<UInt8>,
    _ charlen: UInt64,
    _ at: UInt64,
    _ retStart: UnsafeMutablePointer<UInt64>,
    _ retEnd: UnsafeMutablePointer<UInt64>
) {
    let text = String(bytes: UnsafeBufferPointer(start: charptr, count: Int(charlen)), encoding: .utf8)!
    let recognizer = NLLanguageRecognizer()
    recognizer.processString(text)
    let language = recognizer.dominantLanguage ?? recognizer.languageHypotheses(withMaximum: 1).first!.key
    let tokenizer = NLTokenizer(unit: .word)
    tokenizer.setLanguage(language)
    tokenizer.string = text
    let tokenRange = tokenizer.tokenRange(at: String.Index(utf16Offset: Int(at), in: text))
    
    retStart.pointee = UInt64(tokenRange.lowerBound.utf16Offset(in: text))
    retEnd.pointee = UInt64(tokenRange.upperBound.utf16Offset(in: text))
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
