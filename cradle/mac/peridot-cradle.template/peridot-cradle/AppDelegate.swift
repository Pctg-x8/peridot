import Cocoa
import AVFAudio

@NSApplicationMain
final class AppDelegate : NSObject, NSApplicationDelegate {
    func applicationShouldTerminate(_ sender: NSApplication) -> NSApplication.TerminateReply {
        NSApplication.TerminateReply.terminateLater
    }
}
