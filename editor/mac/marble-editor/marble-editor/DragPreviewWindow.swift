import Cocoa

final class DragPreviewWindow: NSPanel {
    static let instance = DragPreviewWindow()

    private init() {
        super.init(
            contentRect: NSRect(x: 0, y: 0, width: 128, height: 128),
            styleMask: [.nonactivatingPanel, .borderless, .utilityWindow],
            backing: .buffered,
            defer: false,
        )

        self.ignoresMouseEvents = true
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

    func updateRect(_ rect: NSRect) {
        self.setFrame(rect, display: false)
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

@_cdecl("ni_update_drag_preview_rect")
func updateDragPreviewRect(
    onSurface: UnsafeMutableRawPointer?, x: Double, y: Double, width: Double, height: Double
) {
    let result = {
        if let onSurface = onSurface {
            let onSurface = WindowLink.fromBorrowedPtr(onSurface)
            return onSurface.convertPoint(toScreen: NSPoint(x: x, y: onSurface.frame.height - y))
        } else {
            // そのままスクリーン座標が来る
            return NSPoint(x: x, y: y)
        }
    }()

    DragPreviewWindow.instance.updateRect(
        NSRect(x: result.x, y: result.y - height, width: width, height: height)
    )
}
