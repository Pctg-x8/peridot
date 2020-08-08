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
        
        let eventTypes: NSEvent.EventTypeMask = [
            .keyDown, .keyUp, .flagsChanged,
            .mouseMoved,
            .leftMouseDown, .leftMouseUp,
            .rightMouseDown, .rightMouseUp,
            .otherMouseDown, .otherMouseUp,
            .scrollWheel, .magnify, .smartMagnify
        ]
        NSEvent.addLocalMonitorForEvents(matching: eventTypes) { event in
            switch event.type {
            case .keyDown:
                if !event.isARepeat { NSLog("KeyDown event with \(event)") }
            case .keyUp:
                if !event.isARepeat { NSLog("KeyUp event with \(event)") }
            case .flagsChanged:
                if !event.isARepeat { NSLog("FlagsChanged event with \(event)") }
            case .mouseMoved:
                NSLog("MouseMove event with \(event)")
            case .leftMouseDown:
                NSLog("LeftMouseDown event with \(event)")
            case .leftMouseUp:
                NSLog("LeftMouseUp event with \(event)")
            case .rightMouseDown:
                NSLog("RightMouseDown event with \(event)")
            case .rightMouseUp:
                NSLog("RightMouseUp event with \(event)")
            case .otherMouseDown:
                NSLog("OtherMouseDown event with \(event)")
            case .otherMouseUp:
                NSLog("OtherMouseUp event with \(event)")
            case .scrollWheel:
                NSLog("ScrollWheel event with \(event)")
            case .magnify:
                NSLog("Magnify event with \(event)")
            case .smartMagnify:
                NSLog("SmartMagnify event with \(event)")
            default:
                NSLog("Unhandled event with \(event)")
            }
            return event
        }
        
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
