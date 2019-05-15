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
    var workDispatcher: DispatchSourceUserDataAdd? = nil
    
    func initDispatchers() {
        func onUpdateDisplay(_ _: CVDisplayLink,
                             _ inNow: UnsafePointer<CVTimeStamp>,
                             _ inOutputTime: UnsafePointer<CVTimeStamp>,
                             _ flagsIn: CVOptionFlags,
                             _ flagsOut: UnsafeMutablePointer<CVOptionFlags>,
                             _ context: UnsafeMutableRawPointer?) -> CVReturn {
            let self_ = unsafeBitCast(context, to: PeridotRenderableViewController.self)
            self_.workDispatcher!.add(data: 1)
            return kCVReturnSuccess
        }
        let workDispatcher = DispatchSource.makeUserDataAddSource(queue: DispatchQueue.main)
        workDispatcher.setEventHandler(handler: { () in self.enginePointer!.update() })
        self.workDispatcher = workDispatcher
        CVDisplayLinkCreateWithActiveCGDisplays(&self.dplink)
        CVDisplayLinkSetOutputCallback(self.dplink!, onUpdateDisplay,
                                       unsafeBitCast(self, to: UnsafeMutableRawPointer.self))
        CVDisplayLinkSetCurrentCGDisplay(self.dplink!, CGMainDisplayID())
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.enginePointer = NativeGameEngine(forView: &self.view)
        self.view.window?.title = NativeGameEngine.captionbarText()! as String
        initDispatchers()
        (self.view as! PeridotRenderableView).enginePointer = self.enginePointer
    }
    override func viewDidAppear() {
        super.viewDidAppear()
        NSLog("BeginTimer")
        if let d = self.workDispatcher { d.resume() }
        if let d = self.dplink { CVDisplayLinkStart(d) }
    }
    override func viewWillDisappear() {
        super.viewWillDisappear()
        NSLog("ViewWillDisappear")
        if let d = self.dplink {
            NSLog("Stopping Timer")
            let rv = CVDisplayLinkStop(d)
            NSLog("Stopped Timer with %d", rv)
        }
        if let d = self.workDispatcher { d.cancel() }
    }
}
