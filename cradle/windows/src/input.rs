use log::*;
use parking_lot::RwLock;
use peridot::{NativeAnalogInput, NativeButtonInput};
use windows::Win32::Foundation::{ERROR_DEVICE_NOT_CONNECTED, HWND, LPARAM, POINT};
use windows::Win32::UI::Input::KeyboardAndMouse::{
    MapVirtualKeyA, MAPVK_VK_TO_CHAR, VK_BACK, VK_CAPITAL, VK_CONTROL, VK_DOWN, VK_ESCAPE, VK_F1,
    VK_F24, VK_LCONTROL, VK_LEFT, VK_LMENU, VK_LSHIFT, VK_LWIN, VK_MENU, VK_NUMPAD0, VK_NUMPAD9,
    VK_RCONTROL, VK_RETURN, VK_RIGHT, VK_RMENU, VK_RSHIFT, VK_RWIN, VK_SHIFT, VK_SPACE, VK_UP,
};
use windows::Win32::UI::Input::XboxController::{
    XInputEnable, XInputGetState, XINPUT_GAMEPAD_A, XINPUT_GAMEPAD_B, XINPUT_GAMEPAD_BACK,
    XINPUT_GAMEPAD_DPAD_DOWN, XINPUT_GAMEPAD_DPAD_LEFT, XINPUT_GAMEPAD_DPAD_RIGHT,
    XINPUT_GAMEPAD_DPAD_UP, XINPUT_GAMEPAD_LEFT_SHOULDER, XINPUT_GAMEPAD_LEFT_THUMB,
    XINPUT_GAMEPAD_RIGHT_SHOULDER, XINPUT_GAMEPAD_RIGHT_THUMB, XINPUT_GAMEPAD_START,
    XINPUT_GAMEPAD_X, XINPUT_GAMEPAD_Y, XINPUT_STATE,
};
use windows::Win32::UI::Input::{
    GetRawInputData, RegisterRawInputDevices, HRAWINPUT, RAWINPUT, RAWINPUTDEVICE,
    RAWINPUTDEVICE_FLAGS, RAWINPUTHEADER, RIDEV_NOLEGACY, RID_INPUT, RIM_TYPEKEYBOARD,
    RIM_TYPEMOUSE,
};
use windows::Win32::UI::WindowsAndMessaging::{GetCursorPos, RI_KEY_BREAK};

use crate::ThreadsafeWindowOps;

use peridot::mthelper::SharedRef;

pub struct RawInputHandler {}
impl RawInputHandler {
    pub fn init() -> Self {
        let ri_devices = [
            // Generic HID mouse
            RAWINPUTDEVICE {
                usUsagePage: 0x01,
                usUsage: 0x02,
                dwFlags: RAWINPUTDEVICE_FLAGS(0),
                hwndTarget: HWND(0),
            },
            // Generic HID keyboard
            RAWINPUTDEVICE {
                usUsagePage: 0x01,
                usUsage: 0x06,
                dwFlags: RIDEV_NOLEGACY,
                hwndTarget: HWND(0),
            },
        ];
        unsafe {
            RegisterRawInputDevices(&ri_devices, std::mem::size_of::<RAWINPUTDEVICE>() as _)
                .expect("RegisterRawInputDevices failed!")
        };

        RawInputHandler {}
    }

    pub fn handle_wm_input(&mut self, p: &mut peridot::InputProcess, lp: LPARAM) {
        let mut buffer_size = 0;
        unsafe {
            GetRawInputData(
                std::mem::transmute::<_, HRAWINPUT>(lp),
                RID_INPUT,
                None,
                &mut buffer_size,
                std::mem::size_of::<RAWINPUTHEADER>() as _,
            )
        };
        if buffer_size == 0 {
            return;
        }
        let mut buffer = vec![0u8; buffer_size as usize];
        unsafe {
            GetRawInputData(
                std::mem::transmute::<_, HRAWINPUT>(lp),
                RID_INPUT,
                Some(buffer.as_mut_ptr() as *mut _),
                &mut buffer_size,
                std::mem::size_of::<RAWINPUTHEADER>() as _,
            )
        };
        let rinput = unsafe { &*(buffer.as_ptr() as *const RAWINPUT) };

        match rinput.header.dwType {
            t if t == RIM_TYPEKEYBOARD.0 => {
                let kd = unsafe { &rinput.data.keyboard };
                /*debug!(
                    "Keyboard Message! make={} flags={} reserved={} extinfo={} message={} vkey={}",
                    kd.MakeCode,
                    kd.Flags,
                    kd.Reserved,
                    kd.ExtraInformation,
                    kd.Message,
                    kd.VKey
                );*/
                let is_press = (kd.Flags as u32 & RI_KEY_BREAK) == 0;
                let ty = match kd.VKey as i32 {
                    v if v == VK_BACK.0 as _ => NativeButtonInput::Backspace,
                    v if v == VK_RETURN.0 as _ => NativeButtonInput::Enter,
                    v if v == VK_LSHIFT.0 as _ => NativeButtonInput::LeftShift,
                    v if v == VK_RSHIFT.0 as _ => NativeButtonInput::RightShift,
                    v if v == VK_LCONTROL.0 as _ => NativeButtonInput::LeftControl,
                    v if v == VK_RCONTROL.0 as _ => NativeButtonInput::RightControl,
                    v if v == VK_LWIN.0 as _ => NativeButtonInput::LeftMeta,
                    v if v == VK_RWIN.0 as _ => NativeButtonInput::RightMeta,
                    v if v == VK_LMENU.0 as _ => NativeButtonInput::LeftAlt,
                    v if v == VK_RMENU.0 as _ => NativeButtonInput::RightAlt,
                    v if v == VK_CAPITAL.0 as _ => NativeButtonInput::CapsLock,
                    v if v == VK_ESCAPE.0 as _ => NativeButtonInput::Esc,
                    v if v == VK_SPACE.0 as _ => NativeButtonInput::Character(' '),
                    v if v == VK_LEFT.0 as _ => NativeButtonInput::LeftArrow,
                    v if v == VK_RIGHT.0 as _ => NativeButtonInput::RightArrow,
                    v if v == VK_UP.0 as _ => NativeButtonInput::UpArrow,
                    v if v == VK_DOWN.0 as _ => NativeButtonInput::DownArrow,
                    c if ((b'0' as i32)..=(b'9' as i32)).contains(&c) => {
                        NativeButtonInput::Character(c as u8 as _)
                    }
                    c if ((b'A' as i32)..=(b'Z' as i32)).contains(&c) => {
                        NativeButtonInput::Character(c as u8 as _)
                    }
                    c if ((b'a' as i32)..=(b'z' as i32)).contains(&c) => {
                        NativeButtonInput::Character((c as u8 as char).to_ascii_uppercase())
                    }
                    c if (VK_NUMPAD0.0 as _..=VK_NUMPAD9.0 as _).contains(&c) => {
                        NativeButtonInput::Character((b'0' + (c - VK_NUMPAD0.0 as i32) as u8) as _)
                    }
                    c if (VK_F1.0 as _..=VK_F24.0 as _).contains(&c) => {
                        NativeButtonInput::FunctionKey(1 + (c - VK_F1.0 as i32) as u8)
                    }
                    // multi emu
                    v if v == VK_SHIFT.0 as _ => {
                        p.dispatch_button_event(NativeButtonInput::LeftShift, is_press);
                        p.dispatch_button_event(NativeButtonInput::RightShift, is_press);
                        return;
                    }
                    v if v == VK_CONTROL.0 as _ => {
                        p.dispatch_button_event(NativeButtonInput::LeftControl, is_press);
                        p.dispatch_button_event(NativeButtonInput::RightControl, is_press);
                        return;
                    }
                    v if v == VK_MENU.0 as _ => {
                        p.dispatch_button_event(NativeButtonInput::LeftAlt, is_press);
                        p.dispatch_button_event(NativeButtonInput::RightAlt, is_press);
                        return;
                    }
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
            }
            t if t == RIM_TYPEMOUSE.0 => {
                let md = unsafe { &rinput.data.mouse };
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
                for x in 0..8 {
                    if (unsafe { md.Anonymous.Anonymous.usButtonFlags } & (1 << (x * 2 + 0))) != 0 {
                        // Mouse Button Down
                        p.dispatch_button_event(NativeButtonInput::Mouse(x), true);
                    }
                    if (unsafe { md.Anonymous.Anonymous.usButtonFlags } & (1 << (x * 2 + 1))) != 0 {
                        // Mouse Button Up
                        p.dispatch_button_event(NativeButtonInput::Mouse(x), false);
                    }
                }
                if md.lLastX != 0 {
                    p.dispatch_analog_event(NativeAnalogInput::MouseX, md.lLastX as _, false);
                }
                if md.lLastY != 0 {
                    p.dispatch_analog_event(NativeAnalogInput::MouseY, md.lLastY as _, false);
                }
            }
            ut => {
                debug!("Unknown input: {ut}");
            }
        }
    }
}

pub struct NativeInputHandler {
    target_hw: SharedRef<ThreadsafeWindowOps>,
    xi_handler: RwLock<XInputHandler>,
}
impl NativeInputHandler {
    pub fn new(hw: SharedRef<ThreadsafeWindowOps>) -> Self {
        Self {
            target_hw: hw,
            xi_handler: RwLock::new(XInputHandler::new()),
        }
    }
}
impl peridot::NativeInput for NativeInputHandler {
    fn get_pointer_position(&self, index: u32) -> Option<(f32, f32)> {
        if index != 0 {
            return None;
        }

        let mut p0 = [POINT { x: 0, y: 0 }];
        unsafe {
            GetCursorPos(&mut p0[0]).expect("Failed to get cursor pos");
            self.target_hw.map_points_from_desktop(&mut p0);
        }
        Some((p0[0].x as _, p0[0].y as _))
    }

    fn pull(&mut self, p: peridot::NativeEventReceiver) {
        self.xi_handler.write().process_state_changes(p);
    }
}

pub struct XInputHandler {
    current_state: [Option<XINPUT_STATE>; Self::MAX_CONTROLLERS],
}
impl XInputHandler {
    const MAX_CONTROLLERS: usize = 4;

    pub fn new() -> Self {
        unsafe {
            XInputEnable(true);
        }

        XInputHandler {
            current_state: [None; Self::MAX_CONTROLLERS],
        }
    }

    pub fn process_state_changes(&mut self, mut p: peridot::NativeEventReceiver) {
        for n in 0..Self::MAX_CONTROLLERS {
            let mut new_state = std::mem::MaybeUninit::<XINPUT_STATE>::uninit();
            let r = unsafe { XInputGetState(n as _, new_state.as_mut_ptr()) };
            let connected = r != ERROR_DEVICE_NOT_CONNECTED.0;
            let new_state = unsafe { new_state.assume_init() };

            if let Some(old_state) = self.current_state[n].take() {
                if !connected {
                    // disconnected controller
                    info!("Disconnected XInput Controller from #{}", n);
                    Self::dispatch_diff(
                        &old_state,
                        unsafe { &std::mem::MaybeUninit::zeroed().assume_init() },
                        &mut p,
                    );
                } else if old_state.dwPacketNumber != new_state.dwPacketNumber {
                    // has changes
                    Self::dispatch_diff(&old_state, &new_state, &mut p);
                }
            } else if connected {
                // new connected controller
                info!("Connected XInput Controller at #{}", n);
                Self::dispatch_diff(
                    unsafe { &std::mem::MaybeUninit::zeroed().assume_init() },
                    &new_state,
                    &mut p,
                );
            }

            self.current_state[n] = if connected { Some(new_state) } else { None };
        }
    }

    fn dispatch_diff(
        old_state: &XINPUT_STATE,
        new_state: &XINPUT_STATE,
        p: &mut peridot::NativeEventReceiver,
    ) {
        let button_diff_bits = new_state.Gamepad.wButtons.0 ^ old_state.Gamepad.wButtons.0;
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
            (XINPUT_GAMEPAD_RIGHT_SHOULDER, NativeButtonInput::ButtonR),
        ] {
            if (button_diff_bits & bit.0) != 0 {
                p.dispatch_button_event(ity, (new_state.Gamepad.wButtons.0 & bit.0) != 0);
            }
        }

        if new_state.Gamepad.bLeftTrigger != old_state.Gamepad.bLeftTrigger {
            p.dispatch_analog_event(
                NativeAnalogInput::LeftTrigger,
                new_state.Gamepad.bLeftTrigger as f32 / 255.0,
                true,
            );
        }
        if new_state.Gamepad.bRightTrigger != old_state.Gamepad.bRightTrigger {
            p.dispatch_analog_event(
                NativeAnalogInput::RightTrigger,
                new_state.Gamepad.bRightTrigger as f32 / 255.0,
                true,
            );
        }
        if new_state.Gamepad.sThumbLX != old_state.Gamepad.sThumbLX {
            p.dispatch_analog_event(
                NativeAnalogInput::StickX(0),
                normalize_short(new_state.Gamepad.sThumbLX),
                true,
            );
        }
        if new_state.Gamepad.sThumbLY != old_state.Gamepad.sThumbLY {
            p.dispatch_analog_event(
                NativeAnalogInput::StickY(0),
                normalize_short(new_state.Gamepad.sThumbLY),
                true,
            );
        }
        if new_state.Gamepad.sThumbRX != old_state.Gamepad.sThumbRX {
            p.dispatch_analog_event(
                NativeAnalogInput::StickX(1),
                normalize_short(new_state.Gamepad.sThumbRX),
                true,
            );
        }
        if new_state.Gamepad.sThumbRY != old_state.Gamepad.sThumbRY {
            p.dispatch_analog_event(
                NativeAnalogInput::StickY(1),
                normalize_short(new_state.Gamepad.sThumbRY),
                true,
            );
        }
    }
}

#[inline]
fn normalize_short(x: i16) -> f32 {
    if x > 0 {
        x as f32 / 32767.0
    } else {
        -(-(x as f32) / 32768.0)
    }
}
