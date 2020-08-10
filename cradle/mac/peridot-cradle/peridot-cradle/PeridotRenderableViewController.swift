//
//  PeridotRenderableViewController.swift
//  peridot-cradle-mac: Controls native engine
//
//  Created by S.Percentage on 2018/12/01.
//  Copyright © 2018 S.Percentage. All rights reserved.
//

import Foundation
import Cocoa
import Carbon

final class CurrentKeyboardLayoutCodeConverter {
    static let MAX_CHAR_LENGTH: Int = 4
    private var keyboardLayout: UnsafePointer<UCKeyboardLayout>
    
    init() {
        let isource = TISGetInputSourceProperty(TISCopyCurrentKeyboardInputSource().takeRetainedValue(), kTISPropertyUnicodeKeyLayoutData)
        let isourceRef = unsafeBitCast(isource, to: CFData.self)
        let isourceBytes = CFDataGetBytePtr(isourceRef)
        self.keyboardLayout = unsafeBitCast(isourceBytes, to: UnsafePointer<UCKeyboardLayout>.self)
    }
    
    func translate(_ code: UInt16) -> Optional<UnsafeMutablePointer<UniChar>> {
        var deadKeyMask: UInt32 = 0
        var charLength = 0
        let charName = UnsafeMutablePointer<UniChar>.allocate(capacity: Self.MAX_CHAR_LENGTH)
        let r = UCKeyTranslate(
            self.keyboardLayout,
            code,
            UInt16(kUCKeyActionDown),
            0,
            UInt32(LMGetKbdType()),
            UInt32(kUCKeyTranslateNoDeadKeysMask),
            &deadKeyMask,
            Self.MAX_CHAR_LENGTH, &charLength, charName
        )
        if r == noErr { return charName } else { return nil }
    }
}

final class PeridotRenderableViewController : NSViewController {
    var dplink: CVDisplayLink? = nil
    var enginePointer: NativeGameEngine? = nil
    var workDispatcher: DispatchSourceUserDataAdd? = nil
    var clientMousePoint = CGPoint(x: 0, y: 0)
    
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
        
        if let p = self.view.window?.mouseLocationOutsideOfEventStream {
            self.clientMousePoint = p
        }
        
        let kcTranslator = CurrentKeyboardLayoutCodeConverter()
        var oldFlags: NSEvent.ModifierFlags = NSEvent.ModifierFlags(rawValue: 0)
        
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
                if !event.isARepeat {
                    if let cs = kcTranslator.translate(event.keyCode) {
                        NSLog("CharacterKeyDown: \(String(utf16CodeUnits: cs, count: CurrentKeyboardLayoutCodeConverter.MAX_CHAR_LENGTH))")
                        self.enginePointer?.handleCharacterKeyDown(character: UInt8(cs.move()))
                    }
                }
            case .keyUp:
                if !event.isARepeat {
                    if let cs = kcTranslator.translate(event.keyCode) {
                        NSLog("CharacterKeyUp: \(String(utf16CodeUnits: cs, count: CurrentKeyboardLayoutCodeConverter.MAX_CHAR_LENGTH))")
                        self.enginePointer?.handleCharacterKeyUp(character: UInt8(cs.move()))
                    }
                }
            case .flagsChanged:
                NSLog("FlagsChanged event with \(event)")
                if event.modifierFlags.contains(.shift) && !oldFlags.contains(.shift) {
                    // shift on
                    self.enginePointer?.handleKeymodDown(code: KEYMOD_SHIFT)
                }
                if !event.modifierFlags.contains(.shift) && oldFlags.contains(.shift) {
                    // shift off
                    self.enginePointer?.handleKeymodUp(code: KEYMOD_SHIFT)
                }
                if event.modifierFlags.contains(.option) && !oldFlags.contains(.option) {
                    // opt on
                    self.enginePointer?.handleKeymodDown(code: KEYMOD_OPTION)
                }
                if !event.modifierFlags.contains(.option) && oldFlags.contains(.option) {
                    // opt off
                    self.enginePointer?.handleKeymodUp(code: KEYMOD_OPTION)
                }
                if event.modifierFlags.contains(.command) && !oldFlags.contains(.command) {
                    // cmd on
                    self.enginePointer?.handleKeymodDown(code: KEYMOD_COMMAND)
                }
                if !event.modifierFlags.contains(.command) && oldFlags.contains(.command) {
                    // cmd off
                    self.enginePointer?.handleKeymodUp(code: KEYMOD_COMMAND)
                }
                if event.modifierFlags.contains(.control) && !oldFlags.contains(.control) {
                    // ctrl on
                    self.enginePointer?.handleKeymodDown(code: KEYMOD_CONTROL)
                }
                if !event.modifierFlags.contains(.control) && oldFlags.contains(.control) {
                    // ctrl off
                    self.enginePointer?.handleKeymodUp(code: KEYMOD_CONTROL)
                }
                if event.modifierFlags.contains(.capsLock) && !oldFlags.contains(.capsLock) {
                    // caps on
                    self.enginePointer?.handleKeymodDown(code: KEYMOD_CAPSLOCK)
                }
                if !event.modifierFlags.contains(.capsLock) && oldFlags.contains(.capsLock) {
                    // caps off
                    self.enginePointer?.handleKeymodUp(code: KEYMOD_CAPSLOCK)
                }
                oldFlags = event.modifierFlags
            case .mouseMoved:
                self.clientMousePoint = event.locationInWindow
                NSLog("MouseMove event with \(event)")
            case .leftMouseDown:
                self.enginePointer?.handleMouseButtonDown(index: 0)
            case .leftMouseUp:
                self.enginePointer?.handleMouseButtonUp(index: 0)
            case .rightMouseDown:
                self.enginePointer?.handleMouseButtonDown(index: 1)
            case .rightMouseUp:
                self.enginePointer?.handleMouseButtonUp(index: 1)
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
