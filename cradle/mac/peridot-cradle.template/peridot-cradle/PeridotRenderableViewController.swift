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
        var isource = TISGetInputSourceProperty(TISCopyCurrentKeyboardInputSource().takeRetainedValue(), kTISPropertyUnicodeKeyLayoutData)
        if isource == nil {
            // 日本語レイアウトだと上記だとうまくいかないらしい
            // https://github.com/microsoft/node-native-keymap/blob/4bb080c3e83abca10942aa4fb81e2f7a81bf64db/src/keyboard_mac.mm#L89
            isource = TISGetInputSourceProperty(TISCopyCurrentKeyboardLayoutInputSource().takeRetainedValue(), kTISPropertyUnicodeKeyLayoutData)
        }
        let isourceRef = unsafeBitCast(isource, to: CFData.self)
        let isourceBytes = CFDataGetBytePtr(isourceRef)
        self.keyboardLayout = unsafeBitCast(isourceBytes, to: UnsafePointer<UCKeyboardLayout>.self)
    }
    
    func translate(_ code: UInt16) -> Optional<UnsafeMutablePointer<UniChar>> {
        var deadKeyMask: UInt32 = 0
        var charLength = 0
        let charName = UnsafeMutablePointer<UniChar>.allocate(capacity: Self.MAX_CHAR_LENGTH)
        charName.initialize(repeating: 0, count: Self.MAX_CHAR_LENGTH)
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
    var workDispatcher: DispatchSourceUserDataAdd? = nil
    var clientMousePoint = CGPoint(x: 0, y: 0)
    
    private(set) var nativeGameDriver: NativeGameDriver? = nil
    
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
        workDispatcher.setEventHandler(handler: { [weak self] in self?.nativeGameDriver?.update() })
        self.workDispatcher = workDispatcher
        CVDisplayLinkCreateWithActiveCGDisplays(&self.dplink)
        CVDisplayLinkSetOutputCallback(self.dplink!, onUpdateDisplay,
                                       unsafeBitCast(self, to: UnsafeMutableRawPointer.self))
        CVDisplayLinkSetCurrentCGDisplay(self.dplink!, CGMainDisplayID())
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        launch_game(unsafeBitCast(self, to: UnsafeMutableRawPointer.self), unsafeBitCast(self.view.layer! as! CAMetalLayer, to: UnsafeMutableRawPointer.self))
        self.view.window?.title = captionbarText()! as String
        initDispatchers()
        
        if let p = self.view.window?.mouseLocationOutsideOfEventStream {
            self.clientMousePoint = p
        }
        
        let kcTranslator = CurrentKeyboardLayoutCodeConverter()
        var oldFlags: NSEvent.ModifierFlags = NSEvent.ModifierFlags(rawValue: 0)
        
        let eventTypes: NSEvent.EventTypeMask = [
            .keyDown, .keyUp, .flagsChanged,
            .mouseMoved,
            .leftMouseDown, .leftMouseUp, .leftMouseDragged,
            .rightMouseDown, .rightMouseUp, .rightMouseDragged,
            .otherMouseDown, .otherMouseUp, .otherMouseDragged,
            .scrollWheel, .magnify, .smartMagnify
        ]
        NSEvent.addLocalMonitorForEvents(matching: eventTypes) { [weak self] event in
            switch event.type {
            case .keyDown:
                if !event.isARepeat {
                    if let cs = kcTranslator.translate(event.keyCode) {
                        NSLog("CharacterKeyDown: \(String(utf16CodeUnits: cs, count: CurrentKeyboardLayoutCodeConverter.MAX_CHAR_LENGTH))")
                        self?.nativeGameDriver?.handleKeyDown(character: cs.pointee)
                    }
                }
            case .keyUp:
                if !event.isARepeat {
                    if let cs = kcTranslator.translate(event.keyCode) {
                        NSLog("CharacterKeyUp: \(String(utf16CodeUnits: cs, count: CurrentKeyboardLayoutCodeConverter.MAX_CHAR_LENGTH))")
                        self?.nativeGameDriver?.handleKeyUp(character: cs.pointee)
                    }
                }
            case .flagsChanged:
                NSLog("FlagsChanged event with \(event)")
                if event.modifierFlags.contains(.shift) && !oldFlags.contains(.shift) {
                    // shift on
                    self?.nativeGameDriver?.handleKeyDown(mod: KEYMOD_SHIFT)
                }
                if !event.modifierFlags.contains(.shift) && oldFlags.contains(.shift) {
                    // shift off
                    self?.nativeGameDriver?.handleKeyUp(mod: KEYMOD_SHIFT)
                }
                if event.modifierFlags.contains(.option) && !oldFlags.contains(.option) {
                    // opt on
                    self?.nativeGameDriver?.handleKeyDown(mod: KEYMOD_OPTION)
                }
                if !event.modifierFlags.contains(.option) && oldFlags.contains(.option) {
                    // opt off
                    self?.nativeGameDriver?.handleKeyUp(mod: KEYMOD_OPTION)
                }
                if event.modifierFlags.contains(.command) && !oldFlags.contains(.command) {
                    // cmd on
                    self?.nativeGameDriver?.handleKeyDown(mod: KEYMOD_COMMAND)
                }
                if !event.modifierFlags.contains(.command) && oldFlags.contains(.command) {
                    // cmd off
                    self?.nativeGameDriver?.handleKeyUp(mod: KEYMOD_COMMAND)
                }
                if event.modifierFlags.contains(.control) && !oldFlags.contains(.control) {
                    // ctrl on
                    self?.nativeGameDriver?.handleKeyDown(mod: KEYMOD_CONTROL)
                }
                if !event.modifierFlags.contains(.control) && oldFlags.contains(.control) {
                    // ctrl off
                    self?.nativeGameDriver?.handleKeyUp(mod: KEYMOD_CONTROL)
                }
                if event.modifierFlags.contains(.capsLock) && !oldFlags.contains(.capsLock) {
                    // caps on
                    self?.nativeGameDriver?.handleKeyDown(mod: KEYMOD_CAPSLOCK)
                }
                if !event.modifierFlags.contains(.capsLock) && oldFlags.contains(.capsLock) {
                    // caps off
                    self?.nativeGameDriver?.handleKeyUp(mod: KEYMOD_CAPSLOCK)
                }
                oldFlags = event.modifierFlags
            case .mouseMoved, .leftMouseDragged, .rightMouseDragged, .otherMouseDragged:
                self?.clientMousePoint = event.locationInWindow
                self?.nativeGameDriver?.reportMouseMoveAbs(
                    x: Float(event.locationInWindow.x),
                    y: -Float(event.locationInWindow.y)
                )
            case .leftMouseDown:
                self?.nativeGameDriver?.handleMouseButtonDown(0)
            case .leftMouseUp:
                self?.nativeGameDriver?.handleMouseButtonUp(0)
            case .rightMouseDown:
                self?.nativeGameDriver?.handleMouseButtonDown(1)
            case .rightMouseUp:
                self?.nativeGameDriver?.handleMouseButtonUp(1)
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
        
        (self.view as! PeridotRenderableView).viewController = self
    }
    override func viewDidAppear() {
        super.viewDidAppear()
        NSLog("BeginTimer")
        self.workDispatcher?.resume()
        if let d = self.dplink {
            CVDisplayLinkStart(d)
        }
    }
    override func viewWillDisappear() {
        super.viewWillDisappear()
        NSLog("ViewWillDisappear")
        if let d = self.dplink {
            NSLog("Stopping Timer")
            let rv = CVDisplayLinkStop(d)
            NSLog("Stopped Timer with %d", rv)
        }
        self.workDispatcher?.cancel()
    }
    
    func setGameDriverCallbacks(_ callbacks: UnsafeMutablePointer<GameDriverCallbacks>, contextPtr: UnsafeMutableRawPointer) {
        self.nativeGameDriver = NativeGameDriver(callbacks: callbacks, contextPtr: contextPtr)
    }
    
    func resizeNative(_ size: NSSize) {
        self.nativeGameDriver?.resize(size)
    }
}
