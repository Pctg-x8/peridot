import Cocoa
import os

struct TextInputClientForwarding {
    let ftable: UnsafePointer<TextInputClientForwardingFT>
    let context: UnsafeMutableRawPointer
}

final class MainView : NSView, NSTextInputClient {
    var windowLinkCallbacks: WindowLinkCallbackSet? = nil
    var contentsScale: CGFloat = 1.0
    var textInputClientForwarding: TextInputClientForwarding? = nil
    
    func setup() {
        self.wantsLayer = true
        self.layerContentsRedrawPolicy = .never
        self.layerContentsPlacement = .scaleAxesIndependently
        
        let layer = CAMetalLayer()
        layer.framebufferOnly = false // BackdropBlurやるのに必要（ただgrabしない形のやり方も考えた方が良さそう そっちの方が効率いいはず）
        let scaling = self.convertToBacking(NSSize(width: 1.0, height: 1.0))
        self.contentsScale = min(scaling.width, scaling.height)
        layer.contentsScale = self.contentsScale
        self.layer = layer
    }
    
    override var acceptsFirstResponder: Bool {
        get { return true }
    }
    
    var backingSize: CGSize {
        get {
            return self.convertToBacking(self.frame.size)
        }
    }
    
    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
//        NSLog("Resize \(newSize)")
        let drawableSize = self.convertToBacking(newSize)
        self.windowLinkCallbacks?.notifyResize(drawableSize.width, drawableSize.height)
    }
    
    var actualLayer: CAMetalLayer {
        get {
            return self.layer! as! CAMetalLayer
        }
    }
    
    override func keyDown(with event: NSEvent) {
        self.inputContext!.handleEvent(event)
    }
    
    // NSTextInputClient
    
    func hasMarkedText() -> Bool {
        guard let fw = self.textInputClientForwarding else {
            return false
        }
        
        return fw.ftable.pointee.hasMarkedText(fw.context) != 0
    }
    
    func markedRange() -> NSRange {
        guard let fw = self.textInputClientForwarding else {
            return NSRange(location: NSNotFound, length: 0)
        }
        
        var location: Int64 = 0
        var length: Int64 = 0
        if fw.ftable.pointee.markedRange(fw.context, &location, &length) == 0 {
            return NSRange(location: NSNotFound, length: 0)
        }
        return NSRange(location: Int(location), length: Int(length))
    }
    
    func selectedRange() -> NSRange {
        guard let fw = self.textInputClientForwarding else {
            return NSRange(location: NSNotFound, length: 0)
        }
        
        var location: Int64 = 0
        var length: Int64 = 0
        fw.ftable.pointee.selectedRange(fw.context, &location, &length)
        return NSRange(location: Int(location), length: Int(length))
    }
    
    func setMarkedText(_ string: Any, selectedRange: NSRange, replacementRange: NSRange) {
        let fw = self.textInputClientForwarding!
        
        if let s = string as? NSString {
            fw.ftable.pointee.setMarkedText(
                fw.context,
                s.utf8String,
                Int64(selectedRange.location),
                Int64(selectedRange.length),
                Int64(replacementRange.location),
                Int64(replacementRange.length)
            )
        } else if let s = string as? NSAttributedString {
            s.string.utf8CString.withUnsafeBufferPointer { strptr in
                fw.ftable.pointee.setMarkedText(
                    fw.context,
                    strptr.baseAddress,
                    Int64(selectedRange.location),
                    Int64(selectedRange.length),
                    Int64(replacementRange.location),
                    Int64(replacementRange.length)
                )
            }
        } else {
            logger.error("todo: setMarkedText(invalid call?) \(selectedRange) \(replacementRange)")
        }
    }
    
    func unmarkText() {
        logger.warning("todo: unmarkText")
    }
    
    func validAttributesForMarkedText() -> [NSAttributedString.Key] {
        logger.warning("todo: validAttributesForMarkedText")
        return []
    }
    
    func attributedSubstring(forProposedRange range: NSRange, actualRange: NSRangePointer?) -> NSAttributedString? {
        let fw = self.textInputClientForwarding!
        var subStringPtr: UnsafePointer<CChar>? = nil
        var subStringLength: UInt64 = 0
        
        if let actualRange = actualRange {
            var actualLocation: Int64 = 0
            var actualLength: Int64 = 0
            
            fw.ftable.pointee.substring(
                fw.context,
                Int64(range.location),
                Int64(range.length),
                &actualLocation,
                &actualLength,
                &subStringPtr,
                &subStringLength
            )
            
            actualRange.pointee.location = Int(actualLocation)
            actualRange.pointee.length = Int(actualLength)
        } else {
            fw.ftable.pointee.substring(
                fw.context,
                Int64(range.location),
                Int64(range.length),
                nil,
                nil,
                &subStringPtr,
                &subStringLength
            )
        }
        
        let str = subStringPtr!.withMemoryRebound(to: UInt8.self, capacity: Int(subStringLength)) { castedPtr in
            String(bytes: UnsafeBufferPointer(start: castedPtr, count: Int(subStringLength)), encoding: .utf8)!
        }
        return NSAttributedString(string: str)
    }
    
    func insertText(_ string: Any, replacementRange: NSRange) {
        logger.log("insertText")
        let fw = self.textInputClientForwarding!
        
        if let s = string as? NSString {
            fw.ftable.pointee.insertText(
                fw.context,
                s.utf8String,
                Int64(replacementRange.location),
                Int64(replacementRange.length),
            )
        } else if let s = string as? NSAttributedString {
            s.string.utf8CString.withUnsafeBufferPointer { strptr in
                fw.ftable.pointee.insertText(
                    fw.context,
                    strptr.baseAddress,
                    Int64(replacementRange.location),
                    Int64(replacementRange.length),
                )
            }
        } else {
            logger.warning("todo: insertText(invalid call?) \(replacementRange)")
        }
    }
    
    func characterIndex(for point: NSPoint) -> Int {
        logger.warning("todo: characterIndex")
        return 0
    }
    
    func firstRect(forCharacterRange range: NSRange, actualRange: NSRangePointer?) -> NSRect {
        var x: CFloat = 0.0
        var y: CFloat = 0.0
        var width: CFloat = 0.0
        var height: CFloat = 0.0
        
        if let actualRange = actualRange {
            var actualLocation: Int64 = 0
            var actualLength: Int64 = 0
            
            self.textInputClientForwarding!.ftable.pointee.firstRect(
                self.textInputClientForwarding!.context,
                Int64(range.location),
                Int64(range.length),
                &actualLocation,
                &actualLength,
                &x,
                &y,
                &width,
                &height,
            );
            
            actualRange.pointee.location = Int(actualLocation)
            actualRange.pointee.length = Int(actualLength)
        } else {
            self.textInputClientForwarding!.ftable.pointee.firstRect(
                self.textInputClientForwarding!.context,
                Int64(range.location),
                Int64(range.length),
                nil,
                nil,
                &x,
                &y,
                &width,
                &height,
            );
        }
        
        return self.window!.convertToScreen(NSRect(
            x: CGFloat(x),
            y: self.window!.frame.height - CGFloat(y),
            width: CGFloat(width),
            height: CGFloat(height)
        ))
    }
    
    override func doCommand(by selector: Selector) {
        if !self.tryToPerform(selector, with: self) {
            logger.warning("doCommand fail: \(selector)")
        }
    }
    
    override func moveRight(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, NSEvent.ModifierFlags(), Unicode.Scalar(NSRightArrowFunctionKey)!)
    }
    
    override func moveLeft(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, NSEvent.ModifierFlags(), Unicode.Scalar(NSLeftArrowFunctionKey)!)
    }
    
    override func scrollToBeginningOfDocument(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, NSEvent.ModifierFlags(), Unicode.Scalar(NSHomeFunctionKey)!)
    }
    
    override func scrollToEndOfDocument(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, NSEvent.ModifierFlags(), Unicode.Scalar(NSEndFunctionKey)!)
    }
    
    override func deleteBackward(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, NSEvent.ModifierFlags(), Unicode.Scalar(NSBackspaceCharacter)!)
    }
    
    override func deleteForward(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, NSEvent.ModifierFlags(), Unicode.Scalar(NSDeleteFunctionKey)!)
    }
    
    override func moveRightAndModifySelection(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, .shift, Unicode.Scalar(NSRightArrowFunctionKey)!)
    }
    
    override func moveLeftAndModifySelection(_ sender: Any?) {
        self.windowLinkCallbacks?.notifyKeyDownWithChar(0, .shift, Unicode.Scalar(NSLeftArrowFunctionKey)!)
    }
}
