
use winapi::um::winuser::{
    RegisterRawInputDevices, GetRawInputData, GetCursorPos, MapWindowPoints, MapVirtualKeyA,
    RAWINPUTDEVICE, RAWINPUTHEADER, RAWINPUT, RIDEV_NOLEGACY, RID_INPUT,
    RIM_TYPEKEYBOARD, RIM_TYPEMOUSE,
    MAPVK_VK_TO_CHAR
};
use winapi::um::winuser as wu;
use winapi::um::xinput::{XInputEnable, XInputGetState, XINPUT_STATE};
use winapi::um::errhandlingapi::GetLastError;
use winapi::shared::minwindef::{TRUE, FALSE, LPARAM};
use winapi::shared::windef::{HWND, POINT};
use winapi::shared::winerror::ERROR_DEVICE_NOT_CONNECTED;
use peridot::{NativeButtonInput, NativeAnalogInput};

pub struct RawInputHandler {
}
impl RawInputHandler {
    pub fn init() -> Self {
        let ri_devices = [
            // Generic HID mouse
            RAWINPUTDEVICE {
                usUsagePage: 0x01, usUsage: 0x02, dwFlags: 0,
                hwndTarget: std::ptr::null_mut()
            },
            // Generic HID keyboard
            RAWINPUTDEVICE {
                usUsagePage: 0x01, usUsage: 0x06, dwFlags: RIDEV_NOLEGACY,
                hwndTarget: std::ptr::null_mut()
            }
        ];
        let r = unsafe {
            RegisterRawInputDevices(
                ri_devices.as_ptr(), ri_devices.len() as _, std::mem::size_of::<RAWINPUTDEVICE>() as _
            )
        };
        if r == FALSE {
            let ec = unsafe { GetLastError() };
            error!("RegisterRawInputDevices failed! GetLastError={}", ec);
        }

        RawInputHandler {
        }
    }
    
    pub fn handle_wm_input(&mut self, p: &mut peridot::InputProcess, lp: LPARAM) {
        let mut buffer_size = 0;
        unsafe {
            GetRawInputData(
                std::mem::transmute(lp), RID_INPUT,
                std::ptr::null_mut(), &mut buffer_size, std::mem::size_of::<RAWINPUTHEADER>() as _
            )
        };
        if buffer_size == 0 { return; }
        let mut buffer = vec![0u8; buffer_size as usize];
        unsafe {
            GetRawInputData(
                std::mem::transmute(lp), RID_INPUT,
                buffer.as_mut_ptr() as _, &mut buffer_size, std::mem::size_of::<RAWINPUTHEADER>() as _
            )
        };
        let rinput = unsafe { &*(buffer.as_ptr() as *const RAWINPUT) };
    
        match rinput.header.dwType {
            RIM_TYPEKEYBOARD => {
                let kd = unsafe { rinput.data.keyboard() };
                /*debug!(
                    "Keyboard Message! make={} flags={} reserved={} extinfo={} message={} vkey={}",
                    kd.MakeCode,
                    kd.Flags,
                    kd.Reserved,
                    kd.ExtraInformation,
                    kd.Message,
                    kd.VKey
                );*/
                let is_press = (kd.Flags as u32 & wu::RI_KEY_BREAK) != 0;
                let ty = match kd.VKey as i32 {
                    wu::VK_BACK => NativeButtonInput::Backspace,
                    wu::VK_RETURN => NativeButtonInput::Enter,
                    wu::VK_LSHIFT => NativeButtonInput::LeftShift,
                    wu::VK_RSHIFT => NativeButtonInput::RightShift,
                    wu::VK_LCONTROL => NativeButtonInput::LeftControl,
                    wu::VK_RCONTROL => NativeButtonInput::RightControl,
                    wu::VK_LWIN => NativeButtonInput::LeftMeta,
                    wu::VK_RWIN => NativeButtonInput::RightMeta,
                    wu::VK_LMENU => NativeButtonInput::LeftAlt,
                    wu::VK_RMENU => NativeButtonInput::RightAlt,
                    wu::VK_CAPITAL => NativeButtonInput::CapsLock,
                    wu::VK_ESCAPE => NativeButtonInput::Esc,
                    wu::VK_SPACE => NativeButtonInput::Character(' '),
                    wu::VK_LEFT => NativeButtonInput::LeftArrow,
                    wu::VK_RIGHT => NativeButtonInput::RightArrow,
                    wu::VK_UP => NativeButtonInput::UpArrow,
                    wu::VK_DOWN => NativeButtonInput::DownArrow,
                    c if ((b'0' as i32) ..= (b'9' as i32)).contains(&c) => NativeButtonInput::Character(c as u8 as _),
                    c if ((b'A' as i32) ..= (b'Z' as i32)).contains(&c) => NativeButtonInput::Character(c as u8 as _),
                    c if ((b'a' as i32) ..= (b'z' as i32)).contains(&c) => NativeButtonInput::Character(
                        (c as u8 as char).to_ascii_uppercase()
                    ),
                    c @ wu::VK_NUMPAD0 ..= wu::VK_NUMPAD9 => NativeButtonInput::Character(
                        (b'0' + (c - wu::VK_NUMPAD0) as u8) as _
                    ),
                    c @ wu::VK_F1 ..= wu::VK_F24 => NativeButtonInput::FunctionKey(1 + (c - wu::VK_F1) as u8),
                    // multi emu
                    wu::VK_SHIFT => {
                        p.dispatch_button_event(NativeButtonInput::LeftShift, is_press);
                        p.dispatch_button_event(NativeButtonInput::RightShift, is_press);
                        return;
                    },
                    wu::VK_CONTROL => {
                        p.dispatch_button_event(NativeButtonInput::LeftControl, is_press);
                        p.dispatch_button_event(NativeButtonInput::RightControl, is_press);
                        return;
                    },
                    wu::VK_MENU => {
                        p.dispatch_button_event(NativeButtonInput::LeftAlt, is_press);
                        p.dispatch_button_event(NativeButtonInput::RightAlt, is_press);
                        return;
                    },
                    // others
                    _ => {
                        let c = unsafe { MapVirtualKeyA(kd.VKey as _, MAPVK_VK_TO_CHAR) };
                        if c != 0 {
                            NativeButtonInput::Character((c as u8 as char).to_ascii_uppercase())
                        } else {
                            debug!("Unhandled key input: {}", kd.VKey);
                            return;
                        }
                    }
                };
                p.dispatch_button_event(ty, is_press);
            },
            RIM_TYPEMOUSE => {
                let md = unsafe { rinput.data.mouse() };
                /*debug!(
                    "Mouse Message! flags={} btnFlags={} btnData={} rawButtons={} lastX={} lastY={} extinfo={}",
                    md.usFlags,
                    md.usButtonFlags,
                    md.usButtonData,
                    md.ulRawButtons,
                    md.lLastX,
                    md.lLastY,
                    md.ulExtraInformation
                );*/
                for x in 0 .. 8 {
                    if (md.usButtonFlags & (1 << (x * 2 + 0))) != 0 {
                        // Mouse Button Down
                        p.dispatch_button_event(NativeButtonInput::Mouse(x), true);
                    }
                    if (md.usButtonFlags & (1 << (x * 2 + 1))) != 0 {
                        // Mouse Button Up
                        p.dispatch_button_event(NativeButtonInput::Mouse(x), false);
                    }
                }
                if md.lLastX != 0 { p.dispatch_analog_event(NativeAnalogInput::MouseX, md.lLastX as _, false); }
                if md.lLastY != 0 { p.dispatch_analog_event(NativeAnalogInput::MouseY, md.lLastY as _, false); }
            },
            ut => {
                debug!("Unknown input: {}", ut);
            }
        }
    }
}

pub struct NativeInputHandler {
    target_hw: HWND,
    xi_handler: XInputHandler
}
impl NativeInputHandler {
    pub fn new(hw: HWND) -> Self {
        NativeInputHandler {
            target_hw: hw,
            xi_handler: XInputHandler::new()
        }
    }
}
impl peridot::NativeInput for NativeInputHandler {
    fn get_pointer_position(&self, index: u32) -> Option<(f32, f32)> {
        if index != 0 { return None; }

        let mut p0 = POINT { x: 0, y: 0 };
        unsafe { GetCursorPos(&mut p0); MapWindowPoints(std::ptr::null_mut(), self.target_hw, &mut p0, 1); }
        Some((p0.x as _, p0.y as _))
    }
}

pub struct XInputHandler {
    current_state: [Option<XINPUT_STATE>; Self::MAX_CONTROLLERS]
}
impl XInputHandler {
    const MAX_CONTROLLERS: usize = 4;

    pub fn new() -> Self {
        unsafe { XInputEnable(TRUE); }

        XInputHandler {
            current_state: [None; Self::MAX_CONTROLLERS]
        }
    }

    pub fn process_state_changes(&mut self) {
        for n in 0 .. Self::MAX_CONTROLLERS {
            let mut new_state = std::mem::MaybeUninit::<XINPUT_STATE>::uninit();
            let r = unsafe { XInputGetState(n as _, new_state.as_mut_ptr()) };
            let connected = r != ERROR_DEVICE_NOT_CONNECTED;
            let new_state = unsafe { new_state.assume_init() };

            if let Some(old_state) = self.current_state[n].take() {
                if !connected {
                    // disconnected controller
                    info!("Disconnected Controller #{}", n);
                } else if old_state.dwPacketNumber != new_state.dwPacketNumber {
                    // has changes
                    info!("Controller packet changes #{}", n);
                }
            } else if connected {
                // new connected controller
                info!("Connected Controller #{}", n);
            }

            self.current_state[n] = if connected { Some(new_state) } else { None };
        }
    }
}
