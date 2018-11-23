//! Window/WindowController

use objc::runtime::{Object, Class, Sel};
use objc::declare::ClassDecl;
use appkit::*;
use std::ops::Deref;
use std::mem::transmute;

use view::RenderableView;

#[derive(ObjcObjectBase)]
pub struct WindowController(Object);
impl WindowController {
    pub fn new(initial_frame_rect: NSRect) -> Result<CocoaObject<Self>, ()> {
        let c = Class::get("PeridotWindowController").unwrap_or_else(Self::declare);
        let o = unsafe { CocoaObject::<Self>::from_id(msg_send![c, new])? };
        unsafe { (*o.id()).set_ivar("initial_frame_rect", initial_frame_rect); }
        return Ok(o);
    }
    fn declare() -> &'static Class {
        DeclareObjcClass! {
            class PeridotWindowController : NSViewController {
                ivar initial_frame_rect: NSRect;
                - mut loadView = Self::load_view;
            }
        }
    }

    extern fn load_view(this: &mut Object, _: Sel) {
        let initial_frame_rect: &NSRect = unsafe { this.get_ivar("initial_frame_rect") };

        let mut v = RenderableView::new().expect("RenderableView Construct");
        v.set_frame(initial_frame_rect);
        let _: () = unsafe { msg_send![this, setView: v.id()] };
    }
}
// Class Derivative
impl Deref for WindowController {
    type Target = NSViewController;
    fn deref(&self) -> &NSViewController { unsafe { transmute(self) } }
}
