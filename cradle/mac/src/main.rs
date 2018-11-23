#[macro_use]
extern crate objc;
extern crate appkit; use appkit::*;
#[macro_use] extern crate appkit_derive;

extern crate peridot;
extern crate bedrock as br;
use std::fs::File;
use std::io::Result as IOResult;
use std::rc::Rc;

use objc::runtime::{Object, Class, Sel};
use objc::declare::ClassDecl;

#[allow(non_camel_case_types)] pub type objc_id = *mut Object;
macro_rules! DeclareObjcClass
{
    (class $t: ident : $p: ident { $($content: tt)* }) =>
    {{
        let parent = Class::get(stringify!($p)).expect(concat!("objc class ", stringify!($p), "not found"));
        let mut d = ClassDecl::new(stringify!($t), parent).expect(concat!("Beginning declaring ", stringify!($t)));
        DeclareObjcClass!(#Declaring(d) $($content)*);
        d.register()
    }};
    // void with arg
    (#Declaring($d: expr) $(#[$attr: meta])* - $($name: ident : ($aty: ty))+ = $fr: expr; $($rest: tt)*) =>
    {
        $(#[$attr])* unsafe { $d.add_method(sel!($($name :)+), $fr as extern fn(&Object, Sel $(, $aty)*)); }
        DeclareObjcClass!(#Declaring($d) $($rest)*);
    };
    // void with arg mutable-this
    (#Declaring($d: expr) $(#[$attr: meta])* - mut $($name: ident : ($aty: ty))+ = $fr: expr; $($rest: tt)*) =>
    {
        $(#[$attr])* unsafe { $d.add_method(sel!($($name :)+), $fr as extern fn(&mut Object, Sel $(, $aty)*)); }
        DeclareObjcClass!(#Declaring($d) $($rest)*);
    };
    // void noarg
    (#Declaring($d: expr) $(#[$attr: meta])* - $name: ident = $fr: expr; $($rest: tt)*) =>
    {
        $(#[$attr])* unsafe { $d.add_method(sel!($name), $fr as extern fn(&Object, Sel)); }
        DeclareObjcClass!(#Declaring($d) $($rest)*);
    };
    // void noarg mutable-this
    (#Declaring($d: expr) $(#[$attr: meta])* - mut $name: ident = $fr: expr; $($rest: tt)*) =>
    {
        $(#[$attr])* unsafe { $d.add_method(sel!($name), $fr as extern fn(&mut Object, Sel)); }
        DeclareObjcClass!(#Declaring($d) $($rest)*);
    };
    // full
    (#Declaring($d: expr) - ($rty: ty) $($name: ident : ($aty: ty))+ = $fr: expr; $($rest: tt)*) =>
    {
        unsafe { $d.add_method(sel!($($name :)+), $fr as extern fn(&Object, Sel $(, $aty)*) -> $rty); }
        DeclareObjcClass!(#Declaring($d) $($rest)*);
    };
    // noarg
    (#Declaring($d: expr) - ($rty: ty) $name: ident = $fr: expr; $($rest: tt)*) =>
    {
        unsafe { $d.add_method(sel!($name), $fr as extern fn(&Object, Sel) -> $rty); }
        DeclareObjcClass!(#Declaring($d) $($rest)*);
    };
    (#Declaring($d: expr) $(#[$attr: meta])* ivar $name: ident: $vt: ty; $($rest: tt)*) =>
    {
        $(#[$attr])* { $d.add_ivar::<$vt>(stringify!($name)); }
        DeclareObjcClass!(#Declaring($d) $($rest)*);
    };
    (#Declaring($d: expr)) => {  }
}

mod view;
mod window; use window::WindowController;

fn main() {
    let adel = AppDelegate::new();
    let app = appkit::NSApplication::shared().expect("No NSApplication");
    app.set_delegate(adel.objid());
    app.set_activation_policy(NSApplicationActivationPolicy::Regular);
    app.run();
    println!("Hello, world!");
}

/// Info.plistのCFBundleNameもしくはプロセス名
fn product_name() -> &'static NSString
{
    NSBundle::main().and_then(|b| b.object_for_info_dictionary_key("CFBundleName").ok_or(()))
        .unwrap_or_else(|_| NSProcessInfo::current().unwrap().name())
}

#[derive(ObjcObjectBase)]
struct AppDelegate(Object);
impl AppDelegate {
    fn new() -> CocoaObject<Self> {
        let c = DeclareObjcClass!{
            class PeridotAppDelegate : NSObject {
                ivar rust_instance: usize;
                ivar mainwnd_ptr: objc_id;
                - mut applicationDidFinishLaunching:(objc_id) = Self::did_finishing_launch;
            }
        };
        let ptr: *mut Object = unsafe { msg_send![c, new] };
        if ptr.is_null() { panic!("Unable to create AppDelegate"); }
        return unsafe { CocoaObject::from_id_unchecked(ptr) };
    }

    extern fn did_finishing_launch(this: &mut Object, _: Sel, _: objc_id) {
        let nsapp = NSApplication::shared().expect("Missing NSApplication");
        Self::init_menu(nsapp, product_name().to_str());

        let crect = NSRect { origin: CGPoint { x: 0.0, y: 0.0 }, size: CGSize { width: 640.0, height: 480.0 } };
        let wc = WindowController::new(crect).expect("PeridotWindowController Construct");
        let w = unsafe { NSWindow::with_view_controller_ptr(wc.id()).expect("NSWindow Construct") };
        w.center();
        w.make_key_and_order_front(nsapp.objid());
        unsafe { this.set_ivar("mainwnd_ptr", w.into_id()); }
        nsapp.activate_ignoring_other_apps();
    }

    fn init_menu(nsapp: &NSApplication, appname: &str)
    {
        let about_menu = NSMenuItem::new(&format!("About {}", appname), Some(sel!(orderFrontStandardAboutPanel:)), None)
            .expect("AboutMenuItem Construct");
        let prefs = NSMenuItem::new("Preferences...", None, Some(&NSString::from_str(",").expect("PrefsShortKey")))
            .expect("PrefsMenuItem Construct");
        let services = NSMenuItem::new("Services", None, None).expect("ServicesMenuItem Construct");
        services.set_submenu(&NSMenu::new().expect("ServicesSubMenu Construct"));
        let hide_key = NSString::from_str("h").expect("HideMenuItem ShortKey");
        let hide = NSMenuItem::new(&format!("Hide {}", appname), Some(sel!(hide:)), Some(&hide_key))
            .expect("HideMenuItem Construct");
        let hideother = NSMenuItem::new("Hide Others", Some(sel!(hideOtherApplications:)), None)
            .expect("HideOtherMenuItem Construct");
        let showall = NSMenuItem::new("Show All", Some(sel!(unhideAllApplications:)), None)
            .expect("ShowAllMenuItem Construct");
        let quit_key = NSString::from_str("q").expect("QuitMenuItem ShortKey");
        let quit_menu = NSMenuItem::new(&format!("Quit {}", appname), Some(sel!(terminate:)), Some(&quit_key))
            .expect("QuitMenuItem Construct");
        let mut pmenu = NSMenu::new().expect("PrimaryMenu Construct");
        pmenu.add(&about_menu).add(&NSMenuItem::separator().expect("sep"))
             .add(&prefs).add(&NSMenuItem::separator().expect("sep"))
             .add(&services).add(&NSMenuItem::separator().expect("sep"))
             .add(&hide)
             .add(hideother.set_accelerator(NSEventModifierFlags::COMMAND | NSEventModifierFlags::OPTION, "h"))
             .add(&showall).add(&NSMenuItem::separator().expect("sep"))
             .add(&quit_menu);
        
        let mut menu = NSMenu::new().expect("NSMenu Construct");
        menu.add(NSMenuItem::new("", None, None).expect("PrimaryMenuItem Construct").set_submenu(&pmenu));
        nsapp.set_main_menu(&menu);
    }
}

struct PlatformAssetLoader {}
impl PlatformAssetLoader {
    pub fn new() -> Self {
        PlatformAssetLoader {}
    }
}
impl peridot::AssetLoader for PlatformAssetLoader {
    type Asset = File;
    type StreamingAsset = File;

    fn get(&self, _path: &str, _ext: &str) -> IOResult<File> {
        unimplemented!("MacOSAssetLoader::get");
    }
    fn get_streaming(&self, _path: &str, _ext: &str) -> IOResult<File> {
        unimplemented!("MacOSAssetLoader::get_streaming");
    }
}
struct PlatformRenderTargetHandler(*mut Object);
impl PlatformRenderTargetHandler {
    pub fn new(o: *mut Object) -> Self {
        PlatformRenderTargetHandler(o)
    }
}
impl peridot::PlatformRenderTarget for PlatformRenderTargetHandler {
    fn create_surface(&self, vi: &br::Instance, pd: &br::PhysicalDevice, renderer_queue_family: u32)
            -> br::Result<peridot::SurfaceInfo> {
        let obj = br::Surface::new_macos(vi, self.0 as *const _)?;
        if !pd.surface_support(renderer_queue_family, &obj)? {
            panic!("Vulkan Rendering is not supported by this adapter.");
        }
        return peridot::SurfaceInfo::gather_info(&pd, obj);
    }
    fn current_geometry_extent(&self) -> (usize, usize) {
        let NSRect { size, .. } = unsafe { msg_send![self.0, frame] };
        (size.width as _, size.height as _)
    }
}
pub(crate) struct PlatformInputProcessPlugin { processor: Option<Rc<peridot::InputProcess>> }
impl PlatformInputProcessPlugin {
    fn new() -> Self {
        PlatformInputProcessPlugin { processor: None }
    }
}
impl peridot::InputProcessPlugin for PlatformInputProcessPlugin {
    fn on_start_handle(&mut self, ip: &Rc<peridot::InputProcess>) {
        self.processor = Some(ip.clone());
    }
}
mod glib;
type Game = glib::Game<PlatformAssetLoader, PlatformRenderTargetHandler>;
type Engine = peridot::Engine<Game, PlatformAssetLoader, PlatformRenderTargetHandler>;

fn launch_game(v: *mut Object, ipp: &mut PlatformInputProcessPlugin) -> br::Result<Engine> {
    Engine::launch(Game::NAME, Game::VERSION, PlatformRenderTargetHandler::new(v), PlatformAssetLoader::new(), ipp)
}
