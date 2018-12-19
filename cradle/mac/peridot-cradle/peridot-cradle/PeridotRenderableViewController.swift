//
//  PeridotRenderableViewController.swift
//  peridot-cradle-mac: Controls native engine
//
//  Created by S.Percentage on 2018/12/01.
//  Copyright © 2018 S.Percentage. All rights reserved.
//

import Foundation
import Cocoa

final class PeridotRenderableViewController : NSViewController {
    var dplink: CVDisplayLink? = nil
    var enginePointer: NativeGameEngine? = nil
    
    func startDisplayLink() {
        CVDisplayLinkCreateWithActiveCGDisplays(&self.dplink)
    }
    
    override func viewDidLoad() {
        self.view.wantsLayer = true
        self.view.layerContentsRedrawPolicy = .duringViewResize
        self.enginePointer = NativeGameEngine(forView: UnsafeMutablePointer(&self.view))
    }
    override func viewDidAppear() {
        if let d = self.dplink { CVDisplayLinkStart(d) }
    }
    override func viewWillDisappear() {
        if let d = self.dplink { CVDisplayLinkStop(d) }
    }
}
