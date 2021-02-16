//! Input Handlers

use crate::{udev, kernel_input, epoll::Epoll};
use std::rc::Rc;
use std::cell::RefCell;

pub struct InputNativeLink {
    ws_ref: std::rc::Weak<RefCell<super::X11>>
}
impl InputNativeLink {
	pub fn new(ws: &Rc<RefCell<super::X11>>) -> Self {
		InputNativeLink { ws_ref: std::rc::Rc::downgrade(ws) }
	}
}
impl peridot::NativeInput for InputNativeLink {
    fn get_pointer_position(&self, _index: u32) -> Option<(f32, f32)> {
        if let Some(ws) = self.ws_ref.upgrade() {
            let ptrinfo = {
                let wslock = ws.borrow();
                let ck = xcb::query_pointer(&wslock.con, wslock.mainwnd_id);
                wslock.flush();
                ck.get_reply().expect("Failed to query pointer to xcb")
            };
            if ptrinfo.same_screen() {
                // Note: なぜかLinux/XCBでも5.0だけずれるんですけど！！
                Some((ptrinfo.win_x() as _, ptrinfo.win_y() as f32 - 5.0))
            } else {
                debug!("Fixme: Handle same_screen = false");
                None
            }
        } else {
            None
        }
    }
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
        initial_devices: impl Iterator<Item = (String, EventDevice)>, epoll_id_resv: u64, epoll: &Epoll
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
    pub fn add(&mut self, node: String, device: EventDevice, epoll: &Epoll) -> u64 {
        let id = if let Some(&n) = self.id_free_list.iter().next() {
            self.id_free_list.remove(&n);
            n
        } else {
            self.devices_by_id.len() as u64
        };
        epoll.add_fd(device.fd, libc::EPOLLIN as _, id + self.epoll_id_resv)
            .expect("Failed to register device fd to epoll");
        self.devices_by_id.insert(id, device);
        self.device_id_by_node.insert(node, id);
        
        id
    }
    pub fn remove(&mut self, id: u64, epoll: &Epoll) {
        if let Some(d) = self.devices_by_id.remove(&id) {
            self.id_free_list.insert(id);
            epoll.remove_fd(d.fd).expect("Failed to unregister device fd from epoll");
        }
    }

    pub fn lookup_id_by_node(&self, node: &str) -> Option<u64> { self.device_id_by_node.get(node).copied() }
    pub fn translate_epoll_value(&self, epoll_value: u64) -> u64 { epoll_value - self.epoll_id_resv }
    pub fn get_device(&self, id: u64) -> Option<&EventDevice> { self.devices_by_id.get(&id) }
}

pub struct InputSystem {
	event_device_regex: regex::Regex,
	monitor: udev::Monitor,
	devmgr: EventDeviceManager
}
impl InputSystem {
	pub fn new(epoll: &Epoll, epid_monitor: u64, epid_devices_start: u64) -> Self {
		let udev = udev::Context::new().expect("Failed to initialize udev");
		let enumerator = udev::Enumerate::new(&udev).expect("Failed to create udev Enumerator");
		let target_subsystem_name = std::ffi::CString::new("input").expect("encoding failed");
		enumerator.add_match_subsystem(&target_subsystem_name).expect("Failed to set subsystem filter");
		enumerator.add_match_is_initialized().expect("Failed to set initialized filter");
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

		epoll.add_fd(monitor_fd, libc::EPOLLIN as _, epid_monitor).expect("Failed to add udev monitor fd");

		InputSystem {
			devmgr: EventDeviceManager::new(initial_devices, epid_devices_start, epoll),
			event_device_regex,
			monitor,
		}
	}
	pub fn managed_devices_count(&self) -> usize { self.devmgr.len() }

	pub fn process_monitor_event(&mut self, ep: &Epoll) {
		let device = self.monitor.receive_device().expect("no device received?");
		let devnode_c = match device.devnode() {
			Some(s) => s,
			None => return
		};
		let devnode = devnode_c.to_str().expect("decoding failed");
		if !self.event_device_regex.is_match(devnode) { return; }
		let action = device.action().to_str().expect("action decoding failed");
		let device_name = lookup_device_name(&device).unwrap_or_else(|| String::from("<unknown device name>"));
		println!("Incoming Device Event: {}", action);
		// diag_device(&device);

		match action {
			"add" => {
				let is_mouse = device.property_value(
					unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(b"ID_INPUT_MOUSE\0") }
				).map_or(false, |s| s.to_str() == Ok("1"));
				info!("Registering Input Device: {} ({})", devnode, device_name);
				self.devmgr.add(
					String::from(devnode),
					EventDevice::open(devnode_c, is_mouse).expect("Failed to open added device"),
					ep
				);
			},
			"remove" => if let Some(id) = self.devmgr.lookup_id_by_node(devnode) {
				info!( "Unregistering Input Device: {} ({})", devnode, device_name);
				self.devmgr.remove(id, &ep);
			},
			a => debug!("Unknown device action: {:?}", a)
		}
	}
	pub fn process_device_event(&mut self, input: &peridot::InputProcess, ep_value: u64, x11: &crate::X11) {
		let (ev, is_mouse) = match self.devmgr.get_device(self.devmgr.translate_epoll_value(ep_value)) {
			Some(d) => match d.read() {
				Ok(ev) => (ev, d.is_mouse),
				Err(e) => {
					error!("Failed to read from device: {:?}", e);
					return;
				}
			},
			None => {
				error!("device not found? ev={}", ep_value);
				return;
			}
		};
		
		if ev.type_ == kernel_input::EventType::Synchronize as u16 {
			// ignore dataframe sync
			if ev.code == 0 { return; }

			debug!("syn event: code={}, value={}", ev.code, ev.value);
		} else if ev.type_ == kernel_input::EventType::Key as u16 {
			let is_press = match ev.value {
				0 => false,
				1 => true,
				_ => return
            };
            
            let focus_ck = xcb::get_input_focus(&x11.con);
            let qp_cookie = xcb::query_pointer(&x11.con, x11.mainwnd_id);
            x11.flush();
            let ptr = qp_cookie.get_reply().expect("Failed to query pointer");
            let geometry = x11.mainwnd_geometry();
            let focus = focus_ck.get_reply().expect("Failed to query focus info").focus();

			if let Some(b) = map_key_button(ev.code) {
                if matches!(b, peridot::NativeButtonInput::Mouse(_)) {
                    if !(0 ..= geometry.0 as i16).contains(&ptr.win_x()) ||
                       !(0 ..= geometry.1 as i16).contains(&ptr.win_y()) {
                        // out of window
                        return;
                    }
                } else if focus != x11.mainwnd_id {
                    // Ignore if window doesn't have the input focus.
                    return;
                }
				input.dispatch_button_event(b, is_press);
			} else if let Some(a) = map_analog_key_emulation(ev.code) {
                if focus != x11.mainwnd_id {
                    // Ignore if window doesn't have the input focus.
                    return;
                }
				input.dispatch_analog_event(a, if is_press { 1.0 } else { 0.0 }, true);
			} else {
                if focus != x11.mainwnd_id {
                    // Ignore if window doesn't have the input focus.
                    return;
                }
				debug!("key event: code={}", ev.code);
			}
		} else if ev.type_ == kernel_input::EventType::Relative as u16 {
            let focus_ck = xcb::get_input_focus(&x11.con);
            x11.flush();
            let focus = focus_ck.get_reply().expect("Failed to query focus info").focus();
            if focus != x11.mainwnd_id {
                // Ignore if window doesn't have the input focus.
                return;
            }
			if let Some(b) = if is_mouse { map_mouse_input_rel(ev.code) } else { map_input_rel(ev.code) } {
				input.dispatch_analog_event(b, ev.value as _, false);
			} else {
				debug!("relative event: code={}, value={}", ev.code, ev.value);
			}
		} else if ev.type_ == kernel_input::EventType::Absolute as u16 {
			// ignore misc event from mouse, bitmask of pressed buttons
            if is_mouse && ev.code == kernel_input::AbsoluteAxes::Misc as u16 { return; }
            
            let focus_ck = xcb::get_input_focus(&x11.con);
            x11.flush();
            let focus = focus_ck.get_reply().expect("Failed to query focus info").focus();
            if focus != x11.mainwnd_id {
                // Ignore if window doesn't have the input focus.
                return;
            }

			// Todo: hat -> pov button translation
			if let Some(b) = if is_mouse { map_mouse_input_abs(ev.code) } else { map_input_abs(ev.code) } {
				input.dispatch_analog_event(b, ev.value as _, true);
			} else {
				debug!("absolute event: code={}, value={}", ev.code, ev.value);
			}
		} else {
            let focus_ck = xcb::get_input_focus(&x11.con);
            x11.flush();
            let focus = focus_ck.get_reply().expect("Failed to query focus info").focus();
            if focus != x11.mainwnd_id {
                // Ignore if window doesn't have the input focus.
                return;
            }

			// ignore keyscan misc
			if ev.type_ == kernel_input::EventType::Misc as u16 && ev.code == 4 { return; }
			debug!("Other event: type={}, code={}, value={}", ev.type_, ev.code, ev.value);
		}
	}
}

fn map_analog_key_emulation(key: u16) -> Option<peridot::NativeAnalogInput> {
    if key == 0x138 { return Some(peridot::NativeAnalogInput::LeftTrigger); }
    if key == 0x139 { return Some(peridot::NativeAnalogInput::RightTrigger); }

    None
}
fn map_key_button(key: u16) -> Option<peridot::NativeButtonInput> {
    use peridot::NativeButtonInput::*;
    use peridot::NativeButtonInput::Character as C;

    const KEYBOARD_MAP: &[peridot::NativeButtonInput] = &[
        Esc,
        C('1'), C('2'), C('3'), C('4'), C('5'), C('6'), C('7'), C('8'), C('9'), C('0'), C('-'), C('='),
        Backspace,
        C('\t'), C('Q'), C('W'), C('E'), C('R'), C('T'), C('Y'), C('U'), C('I'), C('O'), C('P'), C('{'), C('}'),
        Enter, LeftControl,
        C('A'), C('S'), C('D'), C('F'), C('G'), C('H'), C('J'), C('K'), C('L'), C(';'), C('\''), C('`'),
        LeftShift, C('\\'),
        C('Z'), C('X'), C('C'), C('V'), C('B'), C('N'), C('M'), C(','), C('.'), C('/'),
        RightShift, C('*'), LeftAlt, C(' '),
        CapsLock,
        FunctionKey(1), FunctionKey(2),
        FunctionKey(3), FunctionKey(4),
        FunctionKey(5), FunctionKey(6),
        FunctionKey(7), FunctionKey(8),
        FunctionKey(9), FunctionKey(10)
    ];
    const GAMEPAD_MAP: &[Option<peridot::NativeButtonInput>] = &[
        Some(ButtonA), Some(ButtonB), Some(ButtonC),
        Some(ButtonX), Some(ButtonY), None/*ButtonZ*/,
        Some(ButtonL), Some(ButtonR),
        None, None,
        Some(ButtonSelect), Some(ButtonStart), None,
        Some(Stick(0)), Some(Stick(1))
    ];

    if (1 ..= 68).contains(&key) { return Some(KEYBOARD_MAP[key as usize - 1]); }
    if key == 85 { return Some(peridot::NativeButtonInput::ZenkakuHankaku); }
    if key == 87 { return Some(peridot::NativeButtonInput::FunctionKey(11)); }
    if key == 88 { return Some(peridot::NativeButtonInput::FunctionKey(12)); }
    if key == 97 { return Some(peridot::NativeButtonInput::RightControl); }
    if key == 100 { return Some(peridot::NativeButtonInput::RightAlt); }
    if key == 103 { return Some(peridot::NativeButtonInput::UpArrow); }
    if key == 105 { return Some(peridot::NativeButtonInput::LeftArrow); }
    if key == 106 { return Some(peridot::NativeButtonInput::RightArrow); }
    if key == 108 { return Some(peridot::NativeButtonInput::DownArrow); }
    if key == 125 { return Some(peridot::NativeButtonInput::LeftMeta); }
    if key == 126 { return Some(peridot::NativeButtonInput::RightMeta); }
    if (183 ..= 194).contains(&key) {
        return Some(peridot::NativeButtonInput::FunctionKey(13 + (key - 183) as u8));
    }
    if (0x130 ..= 0x13e).contains(&key) { return GAMEPAD_MAP[key as usize - 0x130]; }
    
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
