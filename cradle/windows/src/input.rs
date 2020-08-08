
use winapi::um::winuser::{
    RegisterRawInputDevices, GetRawInputData,
    RAWINPUTDEVICE, RAWINPUTHEADER, RAWINPUT, RIDEV_NOLEGACY, RID_INPUT,
    RIM_TYPEKEYBOARD, RIM_TYPEMOUSE
};
use winapi::um::errhandlingapi::GetLastError;
use winapi::shared::minwindef::{FALSE, WPARAM, LPARAM};

pub fn init() {
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
    let r = unsafe { RegisterRawInputDevices(ri_devices.as_ptr(), ri_devices.len() as _, std::mem::size_of::<RAWINPUTDEVICE>() as _) };
    if r == FALSE {
        let ec = unsafe { GetLastError() };
        error!("RegisterRawInputDevices failed! GetLastError={}", ec);
    }
}

pub fn handle_wm_input(wp: WPARAM, lp: LPARAM) {
    let mut buffer_size = 0;
    unsafe { GetRawInputData(std::mem::transmute(lp), RID_INPUT, std::ptr::null_mut(), &mut buffer_size, std::mem::size_of::<RAWINPUTHEADER>() as _) };
    if buffer_size == 0 { return; }
    let mut buffer = vec![0u8; buffer_size as usize];
    unsafe { GetRawInputData(std::mem::transmute(lp), RID_INPUT, buffer.as_mut_ptr() as _, &mut buffer_size, std::mem::size_of::<RAWINPUTHEADER>() as _) };
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
        },
        ut => {
            debug!("Unknown input: {}", ut);
        }
    }
}
