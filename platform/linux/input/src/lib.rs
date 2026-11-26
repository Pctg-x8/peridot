use core::ffi::*;
use std::os::fd::AsRawFd;

#[repr(transparent)]
pub struct EventDevice(c_int);
impl AsRawFd for EventDevice {
    #[inline(always)]
    fn as_raw_fd(&self) -> std::os::unix::prelude::RawFd {
        self.0
    }
}
impl EventDevice {
    #[inline]
    pub fn open(path: &CStr) -> std::io::Result<Self> {
        match unsafe { libc::open(path.as_ptr(), libc::O_RDONLY) } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            r => Ok(Self(r)),
        }
    }

    pub fn read(&self) -> std::io::Result<InputEvent> {
        let mut ev = core::mem::MaybeUninit::uninit();
        match unsafe {
            libc::read(
                self.0,
                ev.as_mut_ptr() as _,
                core::mem::size_of::<InputEvent>(),
            )
        } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            _ => Ok(unsafe { ev.assume_init() }),
        }
    }
}
impl Drop for EventDevice {
    #[tracing::instrument(name = "EventDevice::drop", skip(self))]
    fn drop(&mut self) {
        if unsafe { libc::close(self.0) } < 0 {
            let e = std::io::Error::last_os_error();
            tracing::warn!(reason = ?e, "close fd failed");
        }
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct InputEvent {
    pub time: libc::timeval,
    pub type_: u16,
    pub code: u16,
    pub value: i32,
}

/// Event Types
#[repr(u16)]
pub enum EventType {
    Synchronize = 0x00,
    Key = 0x01,
    Relative = 0x02,
    Absolute = 0x03,
    Misc = 0x04,
    Switch = 0x05,
}

/// Synchronization Events
#[repr(u16)]
pub enum SynchronizationEvent {
    Report = 0,
    Config = 1,
    MTReport = 2,
    Dropped = 3,
}

/// Relative Axis
#[repr(u16)]
pub enum RelativeAxes {
    X = 0x00,
    Y = 0x01,
    Z = 0x02,
    RX = 0x03,
    RY = 0x04,
    RZ = 0x05,
    HorizontalWheel = 0x06,
    Dial = 0x07,
    Wheel = 0x08,
    Misc = 0x09,
}

/// Absolute Axes
#[repr(u16)]
pub enum AbsoluteAxes {
    X = 0x00,
    Y = 0x01,
    Z = 0x02,
    RX = 0x03,
    RY = 0x04,
    RZ = 0x05,
    Throttle = 0x06,
    Rudder = 0x07,
    Wheel = 0x08,
    Gas = 0x09,
    Brake = 0x0a,
    Hat0X = 0x10,
    Hat0Y = 0x11,
    Pressure = 0x18,
    Distance = 0x19,
    TiltX = 0x1a,
    TiltY = 0x1b,
    ToolWidth = 0x1c,
    Volume = 0x20,
    Misc = 0x28,
}

/// Key
#[repr(u16)]
pub enum Key {
    Esc = 1,
    Tab = 15,
    LeftControl = 29,
    LeftShift = 42,
    RightShift = 54,
    LeftAlt = 56,
    RightControl = 97,
    RightAlt = 100,
    Home = 102,
    Up = 103,
    PageUp = 104,
    Left = 105,
    Right = 106,
    End = 107,
    Down = 108,
    PageDown = 109,
    Insert = 110,
    LeftMeta = 125,
    RightMeta = 126,
    MouseLeft = 0x110,
    MouseRight = 0x111,
    MouseMiddle = 0x112,
    MouseSide = 0x113,
    MouseExtra = 0x114,
    MouseForward = 0x115,
    MouseBack = 0x116,
    MouseTask = 0x117,
    Joystick = 0x120,
}
