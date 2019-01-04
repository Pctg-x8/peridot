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
        func onUpdateDisplay(_ _: CVDisplayLink,
                             _ inNow: UnsafePointer<CVTimeStamp>,
                             _ inOutputTime: UnsafePointer<CVTimeStamp>,
                             _ flagsIn: CVOptionFlags,
                             _ flagsOut: UnsafeMutablePointer<CVOptionFlags>,
                             _ context: UnsafeMutableRawPointer?) -> CVReturn {
            let self_ = unsafeBitCast(context, to: PeridotRenderableViewController.self)
            DispatchQueue.main.async { self_.enginePointer!.update() }
            return kCVReturnSuccess
        }
        CVDisplayLinkCreateWithActiveCGDisplays(&self.dplink)
        CVDisplayLinkSetOutputCallback(self.dplink!, onUpdateDisplay,
                                       unsafeBitCast(self, to: UnsafeMutableRawPointer.self))
    }
    
    override func viewDidLoad() {
        self.view.wantsLayer = true
        self.view.layerContentsRedrawPolicy = .duringViewResize
        self.enginePointer = NativeGameEngine(forView: &self.view)
        self.view.window?.title = NativeGameEngine.captionbarText()! as String
        startDisplayLink()
        (self.view as! PeridotRenderableView).enginePointer = self.enginePointer
    }
    override func viewDidAppear() {
        NSLog("BeginTimer")
        if let d = self.dplink { CVDisplayLinkStart(d) }
    }
    override func viewWillDisappear() {
        if let d = self.dplink { CVDisplayLinkStop(d) }
    }
}
