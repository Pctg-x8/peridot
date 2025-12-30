import Cocoa

final class MainView : NSView {
    var windowLinkCallbacks: WindowLinkCallbackSet? = nil
    
    func setup() {
        self.wantsLayer = true
        self.layerContentsRedrawPolicy = .never
        self.layerContentsPlacement = .scaleAxesIndependently
        
        let layer = CAMetalLayer()
        let scaling = self.convertToBacking(NSSize(width: 1.0, height: 1.0))
        layer.contentsScale = min(scaling.width, scaling.height)
        self.layer = layer
    }
    
    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
//        NSLog("Resize \(newSize)")
        let drawableSize = self.convertToBacking(newSize)
        self.windowLinkCallbacks?.notifyResize(UInt32(drawableSize.width), UInt32(drawableSize.height))
    }
    
    var actualLayer: CAMetalLayer {
        get {
            return self.layer! as! CAMetalLayer
        }
    }
}
