//! View/ViewDelegates

use objc::runtime::{Object, Class, Sel, BOOL, YES};
use objc::declare::ClassDecl;
use appkit::*;
use std::mem::transmute;
use std::ops::{Deref, DerefMut};
use super::objc_id;
use super::Engine;

#[derive(ObjcObjectBase)]
pub struct RenderableView(Object);
impl RenderableView {
    fn declare() -> &'static Class {
        extern fn yesman(_: &Object, _: Sel) -> BOOL { YES }

        DeclareObjcClass! {
            class PeridotRenderableView : NSView {
                - (objc_id) makeBackingLayer = Self::make_backing_layer;
                - displayLayer:(objc_id) = Self::display_layer;
                - (BOOL) wantsUpdateLayer = yesman;
                - mut setFrameSize:(NSSize) = Self::set_frame_size;

                ivar engine_ptr: usize;
            }
        }
    }
    pub fn new() -> Result<CocoaObject<Self>, ()> {
        let c = Class::get("PeridotRenderableView").unwrap_or_else(Self::declare);
        let ptr = unsafe { msg_send![c, new] };
        unsafe { CocoaObject::from_id(ptr) }
    }
    pub(crate) fn set_engine_ptr(&mut self, p: usize) {
        unsafe { self.0.set_ivar("engine_ptr", p); }
    }

    extern fn make_backing_layer(_this: &Object, _: Sel) -> objc_id {
        // let view: &NSView = unsafe { transmute(this) };
        let layer = CAMetalLayer::layer().expect("Constructing CAMetalLayer");
        layer.into_id()
    }
    extern fn display_layer(this: &Object, _: Sel, _: objc_id) {
        println!("display layer");
        let p = unsafe { transmute::<usize, *mut Engine>(*this.get_ivar("engine_ptr")) };
        if let Some(e) = unsafe { p.as_mut() } { e.do_update(); }
    }
    extern fn set_frame_size(this: &mut Object, _: Sel, size: NSSize) {
        let _: () = unsafe {
            msg_send![super(this, Class::get("NSView").expect("No NSView?")), setFrameSize: size.clone()]
        };
        let this: &mut Self = unsafe { transmute(this) };
        if let Some(layer) = this.layer_mut() {
            let r = CGRect { origin: CGPoint { x: 0.0, y: 0.0 }, size: size.clone() };
            layer.set_frame(r.clone()); layer.set_bounds(r);
        }
    }
}
// Class Derivative
impl Deref for RenderableView {
    type Target = NSView; fn deref(&self) -> &NSView { unsafe { transmute(self) } }
}
impl DerefMut for RenderableView {
    fn deref_mut(&mut self) -> &mut NSView { unsafe { transmute(self) } }
}
