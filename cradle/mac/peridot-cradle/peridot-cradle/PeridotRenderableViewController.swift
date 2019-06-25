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
    var oldMouseLocation = NSPoint(x: 0, y: 0)
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
        self.view.addTrackingArea(NSTrackingArea(rect: self.view.bounds, options: [.mouseMoved, .activeInActiveApp], owner: self))
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
    
    override func scrollWheel(with event: NSEvent) {
        self.enginePointer?.onScroll(x: Float32(event.scrollingDeltaX), y: Float32(event.scrollingDeltaY))
        // print("ScrollWheel: \(event.scrollingDeltaX) \(event.scrollingDeltaY) \(event.hasPreciseScrollingDeltas)")
    }
    override func magnify(with event: NSEvent) {
        self.enginePointer?.onMagnification(amount: Float32(event.magnification))
        // print("Magnify: \(event.magnification)")
    }
    override func rotate(with event: NSEvent) {
        print("Rotate: \(event.rotation)")
    }
    override func mouseDown(with event: NSEvent) {
        print("MouseDown")
    }
    override func mouseUp(with event: NSEvent) {
        print("MouseUp")
    }
    override func mouseMoved(with event: NSEvent) {
        let dx = event.locationInWindow.x - self.oldMouseLocation.x
        let dy = event.locationInWindow.y - self.oldMouseLocation.y
        self.oldMouseLocation = event.locationInWindow
        self.enginePointer?.onMouseMove(x: Float32(dx), y: Float32(dy))
        // print("MouseMoved: \(event.locationInWindow)")
    }
    override func mouseDragged(with event: NSEvent) {
        let dx = event.locationInWindow.x - self.oldMouseLocation.x
        let dy = event.locationInWindow.y - self.oldMouseLocation.y
        self.oldMouseLocation = event.locationInWindow
        self.enginePointer?.onMouseMove(x: Float32(dx), y: Float32(dy))
        // print("MouseDragged: \(event.locationInWindow)")
    }
    override func touchesBegan(with event: NSEvent) {
        print("TouchesBegan: \(event.touches(for: self.view))")
    }
    override func touchesEnded(with event: NSEvent) {
        print("TouchesEnded")
    }
    override func touchesMoved(with event: NSEvent) {
        print("TouchesMoved")
    }
}
