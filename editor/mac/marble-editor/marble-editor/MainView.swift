import Cocoa

final class MainView : NSView {
    var windowLinkCallbacks: WindowLinkCallbackSet? = nil
    var contentsScale: CGFloat = 1.0
    
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
    
    var backingSize: CGSize {
        get {
            return self.convertToBacking(self.frame.size)
        }
    }
    
    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
//        NSLog("Resize \(newSize)")
        let drawableSize = self.convertToBacking(newSize)
        self.windowLinkCallbacks?.notifyResize(drawableSize.width, drawableSize.height)
    }
    
    var actualLayer: CAMetalLayer {
        get {
            return self.layer! as! CAMetalLayer
        }
    }
}
