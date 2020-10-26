
#[macro_use] extern crate log;

use std::fs::File;
use std::path::PathBuf;
use std::io::Result as IOResult;
use bedrock as br;
use peridot::{EngineEvents, FeatureRequests};
use std::os::unix::io::{AsRawFd, RawFd};

mod udev;
mod epoll;
mod userlib;

pub struct PlatformAssetLoader { basedir: PathBuf }
impl PlatformAssetLoader
{
    fn new() -> Self
    {
        #[cfg(feature = "UseExternalAssetPath")] let basedir = PathBuf::from(env!("PERIDOT_EXTERNAL_ASSET_PATH"));
        #[cfg(not(feature = "UseExternalAssetPath"))] let basedir =
        {
            let mut binloc = std::env::current_exe().expect("Getting exe directory");
            binloc.pop(); binloc.push("assets"); binloc
        };

        trace!("Using Assets in {}", basedir.display());
        PlatformAssetLoader { basedir }
    }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader
{
    type Asset = File;
    type StreamingAsset = File;

    fn get(&self, path: &str, ext: &str) -> IOResult<Self::Asset>
    {
        let mut apath = self.basedir.clone();
        apath.push(path.replace(".", "/")); apath.set_extension(ext);
        return File::open(apath);
    }
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::Asset> { self.get(path, ext) }
}
pub struct WindowHandler { dp: *mut xcb::ffi::xcb_connection_t, vis: xcb::Visualid, wid: xcb::Window }
impl peridot::PlatformRenderTarget for WindowHandler
{
    fn surface_extension_name(&self) -> &'static str { "VK_KHR_xcb_surface" }
    fn create_surface(&self, vi: &br::Instance, pd: &br::PhysicalDevice, renderer_queue_family: u32)
        -> br::Result<peridot::SurfaceInfo>
    {
        if !pd.xcb_presentation_support(renderer_queue_family, self.dp, self.vis)
        {
            panic!("Vulkan Presentation is not supported");
        }
        let so = br::Surface::new_xcb(&vi, self.dp, self.wid)?;
        if !pd.surface_support(renderer_queue_family, &so)?
        {
            panic!("Vulkan Surface is not supported");
        }
        return peridot::SurfaceInfo::gather_info(pd, so);
    }
    fn current_geometry_extent(&self) -> (usize, usize)
    {
        (120, 120)
    }
}
pub struct NativeLink { al: PlatformAssetLoader, wh: WindowHandler }
impl peridot::NativeLinker for NativeLink
{
    type AssetLoader = PlatformAssetLoader;
    type RenderTargetProvider = WindowHandler;

    fn asset_loader(&self) -> &PlatformAssetLoader { &self.al }
    fn render_target_provider(&self) -> &WindowHandler { &self.wh }
}

#[allow(dead_code)]
struct X11
{
    con: xcb::Connection, wm_protocols: xcb::Atom, wm_delete_window: xcb::Atom, vis: xcb::Visualid,
    mainwnd_id: xcb::Window
}
impl X11
{
    fn init() -> Self {
        let (con, screen_index) = xcb::Connection::connect(None).expect("Connecting with xcb");
        let s0 = con.get_setup().roots().nth(screen_index as _).expect("No screen");
        let vis = s0.root_visual();

        let wm_protocols = xcb::intern_atom_unchecked(&con, false, "WM_PROTOCOLS");
        let wm_delete_window = xcb::intern_atom_unchecked(&con, false, "WM_DELETE_WINDOW");
        con.flush();
        let wm_protocols = wm_protocols.get_reply().expect("No WM_PROTOCOLS").atom();
        let wm_delete_window = wm_delete_window.get_reply().expect("No WM_DELETE_WINDOW").atom();

        let title = format!("{} v{}.{}.{}",
            userlib::Game::<NativeLink>::NAME,
            userlib::Game::<NativeLink>::VERSION.0,
            userlib::Game::<NativeLink>::VERSION.1,
            userlib::Game::<NativeLink>::VERSION.2
        );
        let mainwnd_id = con.generate_id();
        xcb::create_window(&con, s0.root_depth(), mainwnd_id, s0.root(), 0, 0, 640, 480, 0,
            xcb::WINDOW_CLASS_INPUT_OUTPUT as _, vis, &[]);
        xcb::change_property(&con, xcb::PROP_MODE_REPLACE as _,
            mainwnd_id, xcb::ATOM_WM_NAME, xcb::ATOM_STRING, 8, title.as_bytes());
        xcb::change_property(&con, xcb::PROP_MODE_APPEND as _,
            mainwnd_id, wm_protocols, xcb::ATOM_ATOM, 32, &[wm_delete_window]);
        con.flush();

        X11 { con, wm_protocols, wm_delete_window, vis, mainwnd_id }
    }
    fn fd(&self) -> RawFd { self.con.as_raw_fd() }
    fn show(&self) {
        xcb::map_window(&self.con, self.mainwnd_id);
        self.con.flush();
    }
    /// Returns false if application has beed exited
    fn process_all_events(&self) -> bool {
        while let Some(ev) = self.con.poll_for_event()
        {
            if (ev.response_type() & 0x7f) == xcb::CLIENT_MESSAGE
            {
                let e: &xcb::ClientMessageEvent = unsafe { xcb::cast_event(&ev) };
                if e.data().data32()[0] == self.wm_delete_window { return false; }
            }
            else
            {
                debug!("Generic Event: {:?}", ev.response_type());
            }
        }
        return true;
    }
}

pub struct GameDriver {
    engine: peridot::Engine<NativeLink>,
    usercode: userlib::Game<NativeLink>
}
impl GameDriver {
    fn new(wh: WindowHandler) -> Self {
        let nl = NativeLink {
            al: PlatformAssetLoader::new(),
            wh
        };
        let mut engine = peridot::Engine::new(
            userlib::Game::<NativeLink>::NAME, userlib::Game::<NativeLink>::VERSION,
            nl, userlib::Game::<NativeLink>::requested_features()
        );
        let usercode = userlib::Game::init(&mut engine);
        engine.postinit();

        GameDriver { engine, usercode }
    }

    fn update(&mut self) { self.engine.do_update(&mut self.usercode); }
}

fn lookup_device_name(d: &udev::Device) -> Option<String> {
    match d.property_value(unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(b"NAME\0") }) {
        Some(n) => Some(String::from(n.to_str().expect("decoding failed"))),
        None => d.parent().as_ref().and_then(lookup_device_name)
    }
}

fn main() {
    env_logger::init();
    let x11 = X11::init();

    let udev = udev::Context::new().expect("Failed to initialize udev");
    let enumerator = udev::Enumerate::new(&udev).expect("Failed to create udev Enumerator");
    let target_subsystem_name = std::ffi::CString::new("input").expect("encoding failed");
    enumerator.add_match_subsystem(&target_subsystem_name).expect("Failed to set subsystem filter");
    enumerator.add_match_is_initialized().expect("FAiled to set initialized filter");
    enumerator.scan_devices().expect("Failed to scan devices");
    println!("Input Device Enumeration: ");
    let event_device_regex = regex::Regex::new("event[0-9]+$").expect("invalid regex");
    for e in enumerator.iter() {
        let syspath = e.name().expect("no name?");
        let device = udev::Device::from_syspath(&udev, syspath).expect("Failed to create udev Device");
        let devnode = match device.devnode() {
            Some(s) => s.to_str().expect("decoding failed"),
            None => continue
        };
        if !event_device_regex.is_match(devnode) { continue; }
        println!("* {}", syspath.to_string_lossy());
        println!(
            "  * name = {}",
            lookup_device_name(&device).unwrap_or_else(|| String::from("<unknown device name>"))
        );
        println!("  * devnode = {}", devnode);
        /*println!("  * initialized ? {}", device.is_initialized());
        for p in device.iter_properties() {
            let name = p.name().expect("no name?").to_str().expect("decoding failed");
            if let Some(v) = p.value() {
                println!("  * {} = {}", name, v.to_str().expect("decoding failed value"));
            } else {
                println!("  * {}", name);
            }
        }
        if let Some(p) = device.parent() {
            println!(
                "    * name = {}",
                p.property_value(&name_property_key).map_or("<none>", |s| s.to_str().expect("decoding failed"))
            );
            println!(
                "    * devtype = {}",
                p.devtype().map_or("<none>", |s| s.to_str().expect("decoding failed"))
            );
            println!(
                "    * devnode = {}",
                p.devnode().map_or("<none>", |s| s.to_str().expect("decoding failed"))
            );
            println!("    * initialized ? {}", p.is_initialized());
            for p in p.iter_properties() {
                let name = p.name().expect("no name?").to_str().expect("decoding failed");
                if let Some(v) = p.value() {
                    println!("    * {} = {}", name, v.to_str().expect("decoding failed value"));
                } else {
                    println!("    * {}", name);
                }
            }
        }*/
    }
    let monitor = udev::Monitor::from_netlink(
        &udev, unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(b"udev\0") }
    ).expect("Failed to create udev monitor");
    monitor.filter_add_match_subsystem_devtype(
        unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(b"input\0") },
        None
    ).expect("Failed to set monitor subsystem filter");
    monitor.filter_update().expect("Failed to update monitor filter");
    monitor.enable_receiving().expect("Failed to set monitor receiving enable");
    let monitor_fd = monitor.fd().expect("Failed to retrieve udev monitor fd");

    let mut gd = GameDriver::new(WindowHandler { dp: x11.con.get_raw_conn(), vis: x11.vis, wid: x11.mainwnd_id });

    let ep = epoll::Epoll::new().expect("Failed to create epoll interface");
    ep.add_fd(x11.fd(), libc::EPOLLIN as _).expect("Failed to add x11 fd");
    ep.add_fd(monitor_fd, libc::EPOLLIN as _).expect("Failed to add udev monitor fd");

    x11.show();
    let mut events = vec![unsafe { std::mem::MaybeUninit::zeroed().assume_init() }; 2];
    'app: loop {
        let count = ep.wait(&mut events, Some(1)).expect("Failed to waiting epoll");
        // TODO: あとでちゃんと待つ(external_fence_fdとか使えばepollで待てそうな気がする)
        if count == 0 { gd.update(); }

        for e in &events[..count as usize] {
            if e.u64 == x11.fd() as _ {
                if !x11.process_all_events() { break 'app; }
            } else if e.u64 == monitor_fd as _ {
                let device = monitor.receive_device().expect("no device received?");
                let devnode = match device.devnode() {
                    Some(s) => s.to_str().expect("decoding failed"),
                    None => continue
                };
                if !event_device_regex.is_match(devnode) { continue; }
                println!("Incoming Device Event: {}", device.action().to_str().expect("decoding failed"));
                for p in device.iter_properties() {
                    let name = p.name().expect("no name?").to_str().expect("decoding failed");
                    if let Some(v) = p.value() {
                        println!("  * {} = {}", name, v.to_str().expect("decoding failed value"));
                    } else {
                        println!("  * {}", name);
                    }
                }
                if let Some(p) = device.parent() {
                    println!(
                        "    * name = {}",
                        p.property_value(unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(b"NAME\0") })
                            .map_or("<none>", |s| s.to_str().expect("decoding failed"))
                    );
                    println!(
                        "    * devtype = {}",
                        p.devtype().map_or("<none>", |s| s.to_str().expect("decoding failed"))
                    );
                    println!(
                        "    * devnode = {}",
                        p.devnode().map_or("<none>", |s| s.to_str().expect("decoding failed"))
                    );
                    println!("    * initialized ? {}", p.is_initialized());
                    for p in p.iter_properties() {
                        let name = p.name().expect("no name?").to_str().expect("decoding failed");
                        if let Some(v) = p.value() {
                            println!("    * {} = {}", name, v.to_str().expect("decoding failed value"));
                        } else {
                            println!("    * {}", name);
                        }
                    }
                }
            }
        }
    }
    // while x11.process_all_events() { gd.update(); }
    println!("Terminating Program...");
}
