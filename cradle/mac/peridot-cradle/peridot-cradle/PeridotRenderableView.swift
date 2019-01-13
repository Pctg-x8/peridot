//
//  PeridotRenderableView.swift
//  peridot-cradle-mac: Customized View which is backing CAMetalLayer.
//
//  Created by S.Percentage on 2018/12/01.
//  Copyright © 2018 S.Percentage. All rights reserved.
//

import Foundation
import Cocoa
import QuartzCore

final class PeridotRenderableView : NSView {
    override func makeBackingLayer() -> CALayer {
        let layer = CAMetalLayer()
        let scaling = self.convertToBacking(NSSize(width: 1.0, height: 1.0))
        layer.contentsScale = min(scaling.width, scaling.height)
        return layer
    }
    override var wantsUpdateLayer: Bool { get { return true } }
    
    var enginePointer: NativeGameEngine? = nil
    
    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        if let l = self.layer {
            print("Resize: ", l.frame.size, "->", newSize)
            let rect = CGRect(origin: CGPoint(), size: newSize)
            l.frame = rect
            l.bounds = rect
        }
        if !self.inLiveResize {
            if let e = self.enginePointer { e.resize(newSize) }
        }
    }
}
