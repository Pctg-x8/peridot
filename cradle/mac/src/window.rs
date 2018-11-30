//! Window/WindowController

use objc::runtime::{Object, Class, Sel, YES};
use objc::declare::ClassDecl;
use appkit::*;
use std::ops::Deref;
use std::mem::transmute;

use view::RenderableView;
use super::{Engine, Game, launch_game, PluginLoader};
use std::marker::PhantomData;

#[derive(ObjcObjectBase)]
pub struct WindowController(Object, PhantomData<Engine>);
impl WindowController {
    pub fn new(initial_frame_rect: NSRect) -> Result<CocoaObject<Self>, ()> {
        let c = Class::get("PeridotWindowController").unwrap_or_else(Self::declare);
        let o = unsafe { CocoaObject::<Self>::from_id(msg_send![c, new])? };
        unsafe { (*o.id()).set_ivar("initial_frame_rect", initial_frame_rect); }
        o.set_title(Game::NAME);
        return Ok(o);
    }
    fn declare() -> &'static Class {
        DeclareObjcClass! {
            class PeridotWindowController : NSViewController {
                ivar initial_frame_rect: NSRect;
                ivar plugins_ptr: usize;
                ivar engine_events_ptr: usize;
                - mut loadView = Self::load_view;
                - mut viewDidLoad = Self::did_view_load;
                - mut dealloc = Self::dealloc;
            }
        }
    }

    extern fn load_view(this: &mut Object, _: Sel) {
        let mut v = RenderableView::new().expect("RenderableView Construct");
        v.set_frame(unsafe { this.get_ivar("initial_frame_rect") });
        let _: () = unsafe { msg_send![this, setView: v.id()] };
    }
    extern fn did_view_load(this: &mut Object, _: Sel) {
        let v: *mut Object = unsafe { msg_send![this, view] };
        unsafe { msg_send![v, setWantsLayer: YES] };

        let mut ploader = PluginLoader::new(v);
        let e = launch_game(&mut ploader).expect("Failed to launch the game");
        unsafe {
            let eptr = Box::into_raw(Box::new(e)) as usize;
            let plptr = Box::into_raw(Box::new(ploader)) as usize;
            this.set_ivar("engine_events_ptr", eptr);
            this.set_ivar("plugins_ptr", plptr);
            let vo = transmute::<*mut Object, *mut RenderableView>(v);
            (*vo).set_engine_ptr(eptr);
        }
    }
    extern fn dealloc(this: &mut Object, _: Sel) {
        unsafe {
            let v = unsafe { transmute::<*mut Object, *mut RenderableView>(msg_send![this, view]) };
            unsafe { (*v).set_engine_ptr(0); }
            let ep: usize = *this.get_ivar("engine_events_ptr");
            let plp: usize = *this.get_ivar("plugins_ptr");
            drop(Box::from_raw(ep as *mut Engine));
            drop(Box::from_raw(plp as *mut PluginLoader));
        
            let _: () = msg_send![super(this, Class::get("NSViewController").expect("No NSViewController?")), dealloc];
        }
    }
}
// Class Derivative
impl Deref for WindowController {
    type Target = NSViewController;
    fn deref(&self) -> &NSViewController { unsafe { transmute(self) } }
}
