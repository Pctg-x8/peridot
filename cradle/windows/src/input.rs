
use winapi::um::winuser::{
    RegisterRawInputDevices, GetRawInputData, GetCursorPos, MapWindowPoints,
    RAWINPUTDEVICE, RAWINPUTHEADER, RAWINPUT, RIDEV_NOLEGACY, RID_INPUT,
    RIM_TYPEKEYBOARD, RIM_TYPEMOUSE
};
use winapi::um::errhandlingapi::GetLastError;
use winapi::shared::minwindef::{FALSE, LPARAM};
use winapi::shared::windef::{HWND, POINT};

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
            RegisterRawInputDevices(ri_devices.as_ptr(), ri_devices.len() as _, std::mem::size_of::<RAWINPUTDEVICE>() as _)
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
                debug!(
                    "Keyboard Message! make={} flags={} reserved={} extinfo={} message={} vkey={}",
                    kd.MakeCode,
                    kd.Flags,
                    kd.Reserved,
                    kd.ExtraInformation,
                    kd.Message,
                    kd.VKey
                );
            },
            RIM_TYPEMOUSE => {
                let md = unsafe { rinput.data.mouse() };
                debug!(
                    "Mouse Message! flags={} btnFlags={} btnData={} rawButtons={} lastX={} lastY={} extinfo={}",
                    md.usFlags,
                    md.usButtonFlags,
                    md.usButtonData,
                    md.ulRawButtons,
                    md.lLastX,
                    md.lLastY,
                    md.ulExtraInformation
                );
                for x in 0 .. 8 {
                    if (md.usButtonFlags & (1 << (x * 2 + 0))) != 0 {
                        // Mouse Button Down
                        p.dispatch_button_event(peridot::NativeButtonInput::Mouse(x), true);
                    }
                    if (md.usButtonFlags & (1 << (x * 2 + 1))) != 0 {
                        // Mouse Button Up
                        p.dispatch_button_event(peridot::NativeButtonInput::Mouse(x), false);
                    }
                }
            },
            ut => {
                debug!("Unknown input: {}", ut);
            }
        }
    }
}

pub struct NativeInputHandler {
    target_hw: HWND
}
impl NativeInputHandler {
    pub fn new(hw: HWND) -> Self {
        NativeInputHandler {
            target_hw: hw
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
