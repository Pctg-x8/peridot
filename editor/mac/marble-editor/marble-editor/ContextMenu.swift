import Cocoa
import os

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
}

final class ContextMenuView : NSView {
    var contentsScale: CGFloat = 1.0
    
    func setup() {
        self.wantsLayer = true
        self.layerContentsRedrawPolicy = .never
        self.layerContentsPlacement = .scaleAxesIndependently
        self.autoresizingMask = [.width, .height]
        
        let layer = CAMetalLayer()
        layer.framebufferOnly = false // BackdropBlurやるのに必要（ただgrabしない形のやり方も考えた方が良さそう そっちの方が効率いいはず）
        layer.isOpaque = false
        let scaling = self.convertToBacking(NSSize(width: 1.0, height: 1.0))
        self.contentsScale = min(scaling.width, scaling.height)
        layer.contentsScale = self.contentsScale
        self.layer = layer
    }
    
    var backingSize: CGSize {
        get {
            return self.convertToBacking(self.frame.size)
        }
    }
    
    var actualLayer: CAMetalLayer {
        get {
            return self.layer! as! CAMetalLayer
        }
    }
    
    override func updateTrackingAreas() {
        for a in self.trackingAreas {
            self.removeTrackingArea(a)
        }
        
        self.addTrackingArea(NSTrackingArea(rect: self.bounds, options: [.mouseEnteredAndExited, .activeInActiveApp], owner: self))
    }
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
