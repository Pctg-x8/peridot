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
    override func makeBackingLayer() -> CALayer { return CAMetalLayer() }
    override var wantsUpdateLayer: Bool { get { return true } }
    
    var enginePointer: OpaquePointer? = nil
    
    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        if let l = self.layer {
            let rect = CGRect(origin: CGPoint(), size: newSize)
            l.frame = rect
            l.bounds = rect
        }
    }
}
