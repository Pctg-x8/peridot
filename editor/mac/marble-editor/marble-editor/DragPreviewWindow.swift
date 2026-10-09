import Cocoa

final class DragPreviewWindow : NSPanel {
    static let instance = DragPreviewWindow()
    
    private init() {
        super.init(
            contentRect: NSRect(x: 0, y: 0, width: 128, height: 128),
            styleMask: [.nonactivatingPanel, .borderless, .utilityWindow],
            backing: .buffered,
            defer: false,
        )
        
        self.backgroundColor = NSColor(red: 0.0625, green: 0.6875, blue: 1.0, alpha: 0.0625)
        let view = NSVisualEffectView()
        view.blendingMode = .behindWindow
        view.material = .popover
        view.state = .active
        self.contentView = view
    }
    
    func show(at: NSRect) {
        self.setFrame(at, display: false)
        self.orderFront(self)
    }
    
    func hide() {
        self.orderOut(self)
    }
    
    func move(to: NSPoint) {
        self.setFrameTopLeftPoint(to)
    }
}

@_cdecl("ni_show_drag_preview")
func showDragPreview(x: Double, y: Double, width: Double, height: Double) {
    // top leftの座標が来るのでbottom leftに変換する
    DragPreviewWindow.instance.show(at: NSRect(x: x, y: y - height, width: width, height: height))
}

@_cdecl("ni_hide_drag_preview")
func hideDragPreview() {
    DragPreviewWindow.instance.hide()
}

@_cdecl("ni_move_drag_preview")
func moveDragPreview(x: Double, y: Double) {
    DragPreviewWindow.instance.move(to: NSPoint(x: x, y: y))
}
