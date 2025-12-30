import Cocoa

final class MainView : NSView {
    func setup() {
        self.wantsLayer = true
        self.layerContentsRedrawPolicy = .never
        self.layerContentsPlacement = .scaleAxesIndependently
        
        let layer = CAMetalLayer()
        let scaling = self.convertToBacking(NSSize(width: 1.0, height: 1.0))
        layer.contentsScale = min(scaling.width, scaling.height)
        self.layer = layer
    }
    
    var actualLayer: CAMetalLayer {
        get {
            return self.layer! as! CAMetalLayer
        }
    }
}
