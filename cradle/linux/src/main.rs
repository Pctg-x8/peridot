#![feature(map_first_last)]

#[macro_use] extern crate log;

use std::fs::File;
use std::path::PathBuf;
use std::io::Result as IOResult;
use bedrock as br;
use peridot::{EngineEvents, FeatureRequests};
use std::os::unix::io::{AsRawFd, RawFd};

mod udev;
mod epoll;
mod kernel_input;
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
#[allow(dead_code)]
fn diag_device(d: &udev::Device) {
    for p in d.iter_properties() {
        let name = p.name().expect("no name?").to_str().expect("decoding failed");
        if let Some(v) = p.value() {
            println!("  * {} = {}", name, v.to_str().expect("decoding failed value"));
        } else {
            println!("  * {}", name);
        }
    }
    if let Some(p) = d.parent() {
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

use std::collections::{BTreeMap, BTreeSet, HashMap};

pub struct EventDevice { fd: libc::c_int, is_mouse: bool }
impl EventDevice {
    pub fn open(path: &std::ffi::CStr, is_mouse: bool) -> std::io::Result<Self> {
        let fp = unsafe { libc::open(path.as_ptr(), libc::O_RDONLY) };
        if fp < 0 { Err(std::io::Error::last_os_error()) } else { Ok(Self { fd: fp, is_mouse }) }
    }

    pub fn read(&self) -> std::io::Result<kernel_input::InputEvent> {
        let mut ev = std::mem::MaybeUninit::uninit();
        let r = unsafe { libc::read(self.fd, ev.as_mut_ptr() as _, std::mem::size_of::<kernel_input::InputEvent>()) };
        if r < 0 { Err(std::io::Error::last_os_error()) } else { Ok(unsafe { ev.assume_init() }) }
    }
}
impl Drop for EventDevice {
    fn drop(&mut self) { unsafe { libc::close(self.fd); } }
}

pub struct EventDeviceManager {
    pub epoll_id_resv: u64,
    pub devices_by_id: BTreeMap<u64, EventDevice>,
    pub id_free_list: BTreeSet<u64>,
    pub device_id_by_node: HashMap<String, u64>
}
impl EventDeviceManager {
    pub fn new(
        initial_devices: impl Iterator<Item = (String, EventDevice)>, epoll_id_resv: u64, epoll: &epoll::Epoll
    ) -> Self {
        let mut devices_by_id = BTreeMap::new();
        let mut device_id_by_node = HashMap::with_capacity(initial_devices.size_hint().0);
        for (n, (node, d)) in initial_devices.enumerate() {
            epoll.add_fd(d.fd, libc::EPOLLIN as _, n as u64 + epoll_id_resv)
                .expect("Failed to register initial device fd to epoll");
            devices_by_id.insert(n as u64, d);
            device_id_by_node.insert(node, n as u64);
        }

        EventDeviceManager {
            epoll_id_resv,
            devices_by_id,
            id_free_list: BTreeSet::new(),
            device_id_by_node
        }
    }
    pub fn len(&self) -> usize { self.devices_by_id.len() }

    /// Returns assigned id(in EventDeviceManager)
    pub fn add(&mut self, node: String, device: EventDevice, epoll: &epoll::Epoll) -> u64 {
        let id = self.id_free_list.pop_first().unwrap_or_else(|| self.devices_by_id.len() as u64);
        epoll.add_fd(device.fd, libc::EPOLLIN as _, id + self.epoll_id_resv)
            .expect("Failed to register device fd to epoll");
        self.devices_by_id.insert(id, device);
        self.device_id_by_node.insert(node, id);
        
        id
    }
    pub fn remove(&mut self, id: u64, epoll: &epoll::Epoll) {
        if let Some(d) = self.devices_by_id.remove(&id) {
            self.id_free_list.insert(id);
            epoll.remove_fd(d.fd).expect("Failed to unregister device fd from epoll");
        }
    }

    pub fn lookup_id_by_node(&self, node: &str) -> Option<u64> { self.device_id_by_node.get(node).copied() }
    pub fn translate_epoll_value(&self, epoll_value: u64) -> u64 { epoll_value - self.epoll_id_resv }
    pub fn get_device(&self, id: u64) -> Option<&EventDevice> { self.devices_by_id.get(&id) }
}

pub struct InputNativeLink {
    ws_ref: std::rc::Weak<X11>
}
impl peridot::NativeInput for InputNativeLink {
    fn get_pointer_position(&self, _index: u32) -> Option<(f32, f32)> {
        if let Some(ws) = self.ws_ref.upgrade() {
            let ck = xcb::query_pointer(&ws.con, ws.mainwnd_id);
            ws.con.flush();
            let ptrinfo = ck.get_reply().expect("Failed to query pointer to xcb");
            println!("ptrinfo: same_screen ? {}", ptrinfo.same_screen());
            // Note: なぜかLinux/XCBでも5.0だけずれるんですけど！！
            Some((ptrinfo.win_x() as _, ptrinfo.win_y() as f32 - 5.0))
        } else {
            None
        }
    }
}

fn main() {
    env_logger::init();
    let x11 = std::rc::Rc::new(X11::init());

    let udev = udev::Context::new().expect("Failed to initialize udev");
    let enumerator = udev::Enumerate::new(&udev).expect("Failed to create udev Enumerator");
    let target_subsystem_name = std::ffi::CString::new("input").expect("encoding failed");
    enumerator.add_match_subsystem(&target_subsystem_name).expect("Failed to set subsystem filter");
    enumerator.add_match_is_initialized().expect("FAiled to set initialized filter");
    enumerator.scan_devices().expect("Failed to scan devices");
    let id_input_mouse = unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(b"ID_INPUT_MOUSE\0") };
    let event_device_regex = regex::Regex::new("event[0-9]+$").expect("invalid regex");
    let initial_devices = enumerator.iter().filter_map(|e| {
        let syspath = e.name().expect("no name?");
        let device = udev::Device::from_syspath(&udev, syspath).expect("Failed to create udev Device");
        let devnode_c = match device.devnode() {
            Some(s) => s,
            None => return None
        };
        let devnode = devnode_c.to_str().expect("decoding failed");
        if !event_device_regex.is_match(devnode) { return None; }
        let device_name = lookup_device_name(&device).unwrap_or_else(|| String::from("<unknown device name>"));
        // diag_device(&device);
        let is_mouse = device.property_value(id_input_mouse).map_or(false, |s| s.to_str() == Ok("1"));

        info!("Registering Input Device: {} ({})", devnode, device_name);
        
        Some((String::from(devnode), EventDevice::open(devnode_c, is_mouse).expect("Failed to open event device")))
    });
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
    gd.engine.input_mut().set_nativelink(Box::new(InputNativeLink { ws_ref: std::rc::Rc::downgrade(&x11) }));

    let ep = epoll::Epoll::new().expect("Failed to create epoll interface");
    ep.add_fd(x11.fd(), libc::EPOLLIN as _, 0).expect("Failed to add x11 fd");
    ep.add_fd(monitor_fd, libc::EPOLLIN as _, 1).expect("Failed to add udev monitor fd");
    let mut devmgr = EventDeviceManager::new(initial_devices, 2, &ep);

    x11.show();
    let mut events = vec![unsafe { std::mem::MaybeUninit::zeroed().assume_init() }; 2 + devmgr.len()];
    'app: loop {
        if events.len() != 2 + devmgr.len() {
            // resize
            events.resize(2 + devmgr.len(), unsafe { std::mem::MaybeUninit::zeroed().assume_init() });
        }

        let count = ep.wait(&mut events, Some(1)).expect("Failed to waiting epoll");
        // TODO: あとでちゃんと待つ(external_fence_fdとか使えばepollで待てそうな気がする)
        if count == 0 { gd.update(); }

        for e in &events[..count as usize] {
            if e.u64 == 0 {
                if !x11.process_all_events() { break 'app; }
            } else if e.u64 == 1 {
                let device = monitor.receive_device().expect("no device received?");
                let devnode_c = match device.devnode() {
                    Some(s) => s,
                    None => continue
                };
                let devnode = devnode_c.to_str().expect("decoding failed");
                if !event_device_regex.is_match(devnode) { continue; }
                let action = device.action().to_str().expect("action decoding failed");
                let device_name = lookup_device_name(&device).unwrap_or_else(|| String::from("<unknown device name>"));
                println!("Incoming Device Event: {}", action);
                // diag_device(&device);

                match action {
                    "add" => {
                        let is_mouse = device.property_value(id_input_mouse).map_or(false, |s| s.to_str() == Ok("1"));
                        info!("Registering Input Device: {} ({})", devnode, device_name);
                        devmgr.add(
                            String::from(devnode),
                            EventDevice::open(devnode_c, is_mouse).expect("Failed to open added device"),
                            &ep
                        );
                    },
                    "remove" => if let Some(id) = devmgr.lookup_id_by_node(devnode) {
                        info!( "Unregistering Input Device: {} ({})", devnode, device_name);
                        devmgr.remove(id, &ep);
                    },
                    a => debug!("Unknown device action: {:?}", a)
                }
            } else {
                let (ev, is_mouse) = match devmgr.get_device(devmgr.translate_epoll_value(e.u64)) {
                    Some(d) => match d.read() {
                        Ok(ev) => (ev, d.is_mouse),
                        Err(e) => {
                            error!("Failed to read from device: {:?}", e);
                            continue;
                        }
                    },
                    None => {
                        let evnum = unsafe { std::ptr::read_unaligned(&e.u64) };
                        error!("device not found? ev={}", evnum);
                        continue;
                    }
                };
                
                if ev.type_ == kernel_input::EventType::Synchronize as u16 {
                    // ignore dataframe sync
                    if ev.code == 0 { continue; }

                    println!("syn event: code={}, value={}", ev.code, ev.value);
                } else if ev.type_ == kernel_input::EventType::Key as u16 {
                    let is_press = match ev.value {
                        0 => false,
                        1 => true,
                        _ => continue
                    };

                    if let Some(b) = map_key_button(ev.code) {
                        gd.engine.input().dispatch_button_event(b, is_press);
                    } else {
                        debug!("key event: code={}", ev.code);
                    }
                } else if ev.type_ == kernel_input::EventType::Relative as u16 {
                    if let Some(b) = if is_mouse { map_mouse_input_rel(ev.code) } else { map_input_rel(ev.code) } {
                        gd.engine.input().dispatch_analog_event(b, ev.value as _, false);
                    } else {
                        debug!("relative event: code={}, value={}", ev.code, ev.value);
                    }
                } else if ev.type_ == kernel_input::EventType::Absolute as u16 {
                    // ignore misc event from mouse, bitmask of pressed buttons
                    if is_mouse && ev.code == kernel_input::AbsoluteAxes::Misc as u16 { continue; }
                    
                    if let Some(b) = if is_mouse { map_mouse_input_abs(ev.code) } else { map_input_abs(ev.code) } {
                        gd.engine.input().dispatch_analog_event(b, ev.value as _, true);
                    } else {
                        debug!("absolute event: code={}, value={}", ev.code, ev.value);
                    }
                } else {
                    // ignore keyscan misc
                    if ev.type_ == kernel_input::EventType::Misc as u16 && ev.code == 4 { continue; }
                    debug!("Other event: type={}, code={}, value={}", ev.type_, ev.code, ev.value);
                }
            }
        }
    }
    println!("Terminating Program...");
}

fn map_key_button(key: u16) -> Option<peridot::NativeButtonInput> {
    if (kernel_input::Key::Left as u16 ..= kernel_input::Key::Task as u16).contains(&key) {
        return Some(peridot::NativeButtonInput::Mouse((key - kernel_input::Key::Left as u16) as _));
    }

    debug!("key event: code={}", key);
    None
}
fn map_mouse_input_rel(code: u16) -> Option<peridot::NativeAnalogInput> {
    if code == kernel_input::RelativeAxes::X as u16 {
        return Some(peridot::NativeAnalogInput::MouseX);
    }
    if code == kernel_input::RelativeAxes::Y as u16 {
        return Some(peridot::NativeAnalogInput::MouseY);
    }
    if code == kernel_input::RelativeAxes::Wheel as u16 {
        return Some(peridot::NativeAnalogInput::ScrollWheel);
    }

    map_input_rel(code)
}
fn map_input_rel(code: u16) -> Option<peridot::NativeAnalogInput> {
    if code == kernel_input::RelativeAxes::X as u16 {
        return Some(peridot::NativeAnalogInput::StickX(0));
    }
    if code == kernel_input::RelativeAxes::RX as u16 {
        return Some(peridot::NativeAnalogInput::StickX(1));
    }
    if code == kernel_input::RelativeAxes::Y as u16 {
        return Some(peridot::NativeAnalogInput::StickY(0));
    }
    if code == kernel_input::RelativeAxes::RY as u16 {
        return Some(peridot::NativeAnalogInput::StickY(1));
    }
    if code == kernel_input::RelativeAxes::Z as u16 {
        return Some(peridot::NativeAnalogInput::StickZ(0));
    }
    if code == kernel_input::RelativeAxes::RZ as u16 {
        return Some(peridot::NativeAnalogInput::StickZ(1));
    }

    None
}
fn map_mouse_input_abs(code: u16) -> Option<peridot::NativeAnalogInput> {
    if code == kernel_input::AbsoluteAxes::X as u16 {
        return Some(peridot::NativeAnalogInput::MouseX);
    }
    if code == kernel_input::AbsoluteAxes::Y as u16 {
        return Some(peridot::NativeAnalogInput::MouseY);
    }
    if code == kernel_input::AbsoluteAxes::Wheel as u16 {
        return Some(peridot::NativeAnalogInput::ScrollWheel);
    }

    map_input_abs(code)
}
fn map_input_abs(code: u16) -> Option<peridot::NativeAnalogInput> {
    if code == kernel_input::AbsoluteAxes::X as u16 {
        return Some(peridot::NativeAnalogInput::StickX(0));
    }
    if code == kernel_input::AbsoluteAxes::RX as u16 {
        return Some(peridot::NativeAnalogInput::StickX(1));
    }
    if code == kernel_input::AbsoluteAxes::Y as u16 {
        return Some(peridot::NativeAnalogInput::StickY(0));
    }
    if code == kernel_input::AbsoluteAxes::RY as u16 {
        return Some(peridot::NativeAnalogInput::StickY(1));
    }
    if code == kernel_input::AbsoluteAxes::Z as u16 {
        return Some(peridot::NativeAnalogInput::StickZ(0));
    }
    if code == kernel_input::AbsoluteAxes::RZ as u16 {
        return Some(peridot::NativeAnalogInput::StickZ(1));
    }

    None
}
