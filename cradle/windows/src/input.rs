
use winapi::um::winuser::{
    RegisterRawInputDevices, GetRawInputData, GetCursorPos, MapWindowPoints, MapVirtualKeyA,
    RAWINPUTDEVICE, RAWINPUTHEADER, RAWINPUT, RIDEV_NOLEGACY, RID_INPUT,
    RIM_TYPEKEYBOARD, RIM_TYPEMOUSE,
    MAPVK_VK_TO_CHAR
};
use winapi::um::winuser as wu;
use winapi::um::xinput::*;
use winapi::um::errhandlingapi::GetLastError;
use winapi::shared::minwindef::{TRUE, FALSE, LPARAM};
use winapi::shared::windef::{HWND, POINT};
use winapi::shared::winerror::ERROR_DEVICE_NOT_CONNECTED;
use peridot::{NativeButtonInput, NativeAnalogInput};
use std::cell::RefCell;

pub struct RawInputHandler {x}
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
    xi_handler: RefCell<XInputHandler>
}
impl NativeInputHandler {
    pub fn new(hw: HWND) -> Self {
        NativeInputHandler {
            target_hw: hw,
            xi_handler: RefCell::new(XInputHandler::new())
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
    fn pull(&self, p: &peridot::InputProcess) {
        self.xi_handler.borrow_mut().process_state_changes(p);
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

    pub fn process_state_changes(&mut self, p: &peridot::InputProcess) {
        for n in 0 .. Self::MAX_CONTROLLERS {
            let mut new_state = std::mem::MaybeUninit::<XINPUT_STATE>::uninit();
            let r = unsafe { XInputGetState(n as _, new_state.as_mut_ptr()) };
            let connected = r != ERROR_DEVICE_NOT_CONNECTED;
            let new_state = unsafe { new_state.assume_init() };

            if let Some(old_state) = self.current_state[n].take() {
                if !connected {
                    // disconnected controller
                    info!("Disconnected XInput Controller from #{}", n);
                    Self::dispatch_diff(&old_state, unsafe { &std::mem::MaybeUninit::zeroed().assume_init() }, p);
                } else if old_state.dwPacketNumber != new_state.dwPacketNumber {
                    // has changes
                    Self::dispatch_diff(&old_state, &new_state, p);
                }
            } else if connected {
                // new connected controller
                info!("Connected XInput Controller at #{}", n);
                Self::dispatch_diff(unsafe { &std::mem::MaybeUninit::zeroed().assume_init() }, &new_state, p);
            }

            self.current_state[n] = if connected { Some(new_state) } else { None };
        }
    }

    fn dispatch_diff(old_state: &XINPUT_STATE, new_state: &XINPUT_STATE, p: &peridot::InputProcess) {
        let button_diff_bits = new_state.Gamepad.wButtons ^ old_state.Gamepad.wButtons;
        for &(bit, ity) in &[
            (XINPUT_GAMEPAD_A, NativeButtonInput::ButtonA),
            (XINPUT_GAMEPAD_B, NativeButtonInput::ButtonB),
            (XINPUT_GAMEPAD_X, NativeButtonInput::ButtonX),
            (XINPUT_GAMEPAD_Y, NativeButtonInput::ButtonY),
            (XINPUT_GAMEPAD_START, NativeButtonInput::ButtonStart),
            (XINPUT_GAMEPAD_BACK, NativeButtonInput::ButtonSelect),
            (XINPUT_GAMEPAD_DPAD_UP, NativeButtonInput::POVUp),
            (XINPUT_GAMEPAD_DPAD_DOWN, NativeButtonInput::POVDown),
            (XINPUT_GAMEPAD_DPAD_LEFT, NativeButtonInput::POVLeft),
            (XINPUT_GAMEPAD_DPAD_RIGHT, NativeButtonInput::POVRight),
            (XINPUT_GAMEPAD_LEFT_THUMB, NativeButtonInput::Stick(0)),
            (XINPUT_GAMEPAD_RIGHT_THUMB, NativeButtonInput::Stick(1)),
            (XINPUT_GAMEPAD_LEFT_SHOULDER, NativeButtonInput::ButtonL),
            (XINPUT_GAMEPAD_RIGHT_SHOULDER, NativeButtonInput::ButtonR)
        ] {
            if (button_diff_bits & bit) != 0 {
                p.dispatch_button_event(ity, (new_state.Gamepad.wButtons & bit) != 0);
            }
        }

        if new_state.Gamepad.bLeftTrigger != old_state.Gamepad.bLeftTrigger {
            p.dispatch_analog_event(
                NativeAnalogInput::LeftTrigger,
                new_state.Gamepad.bLeftTrigger as f32 / 255.0,
                true
            );
        }
        if new_state.Gamepad.bRightTrigger != old_state.Gamepad.bRightTrigger {
            p.dispatch_analog_event(
                NativeAnalogInput::RightTrigger,
                new_state.Gamepad.bRightTrigger as f32 / 255.0,
                true
            );
        }
        if new_state.Gamepad.sThumbLX != old_state.Gamepad.sThumbLX {
            p.dispatch_analog_event(
                NativeAnalogInput::StickX(0),
                normalize_short(new_state.Gamepad.sThumbLX),
                true
            );
        }
        if new_state.Gamepad.sThumbLY != old_state.Gamepad.sThumbLY {
            p.dispatch_analog_event(
                NativeAnalogInput::StickY(0),
                normalize_short(new_state.Gamepad.sThumbLY),
                true
            );
        }
        if new_state.Gamepad.sThumbRX != old_state.Gamepad.sThumbRX {
            p.dispatch_analog_event(
                NativeAnalogInput::StickX(1),
                normalize_short(new_state.Gamepad.sThumbRX),
                true
            );
        }
        if new_state.Gamepad.sThumbRY != old_state.Gamepad.sThumbRY {
            p.dispatch_analog_event(
                NativeAnalogInput::StickY(1),
                normalize_short(new_state.Gamepad.sThumbRY),
                true
            );
        }
    }
}

fn normalize_short(x: winapi::shared::ntdef::SHORT) -> f32 {
    if x > 0 { x as f32 / 32767.0 } else { -(-(x as f32) / 32768.0) }
}
