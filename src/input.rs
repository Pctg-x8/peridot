
use std::rc::Rc;
use std::cell::RefCell;
use math::{Vector2F32, Vector2};

#[derive(Debug)]
pub enum InputEventSource {
    KeyboardChar(char), MouseDown(u8), MouseUp(u8)
}
#[derive(Debug)]
pub enum AxisEventSource {
    MousePress(u8), ScrollHorizontal, ScrollVertical, Magnification, MouseMoveHorizontal, MouseMoveVertical
}
pub trait PlatformInputProcessor {
    fn link(&mut self, source: InputEventSource, vinput_num: u32);
    fn link_axis(&mut self, source: AxisEventSource, vaxe_num: u32);
    fn query(&self, vinput_num: u32) -> bool;
    fn query_axis(&self, vaxe_num: u32) -> f32;
}

const MAX_MOUSE_BUTTONS: usize = 5;
struct AsyncCollectedData {
    mouse_position: Vector2F32, mouse_wheel_motion: Vector2F32, mouse_button: [bool; MAX_MOUSE_BUTTONS],
    gesture_magnification: f32
}
#[derive(Debug)]
struct FrameData {
    mouse_position: Vector2F32, prev_mouse_position: Vector2F32,
    mouse_wheel_motion: Vector2F32, mouse_pressing: [bool; MAX_MOUSE_BUTTONS],
    mouse_down_inframe: [bool; MAX_MOUSE_BUTTONS], mouse_up_inframe: [bool; MAX_MOUSE_BUTTONS],
}
pub struct InputProcess {
    collected: RefCell<AsyncCollectedData>, frame: RefCell<FrameData>
}
pub trait InputProcessPlugin {
    fn on_start_handle(&mut self, processor: &Rc<InputProcess>);
}
impl InputProcess {
    pub fn new() -> Self {
        let cd = AsyncCollectedData {
            mouse_position: Vector2(0.0, 0.0),
            mouse_wheel_motion: Vector2(0.0, 0.0), mouse_button: [false; MAX_MOUSE_BUTTONS],
            gesture_magnification: 0.0
        };
        let fd = FrameData {
            mouse_position: Vector2(0.0, 0.0), prev_mouse_position: Vector2(0.0, 0.0),
            mouse_wheel_motion: Vector2(0.0, 0.0), mouse_pressing: [false; MAX_MOUSE_BUTTONS],
            mouse_down_inframe: [false; MAX_MOUSE_BUTTONS], mouse_up_inframe: [false; MAX_MOUSE_BUTTONS]
        };
        return InputProcess { collected: cd.into(), frame: fd.into() };
    }
    pub fn dispatch_message<M: InputMessage>(&self, msg: M) { msg.process(self); }
    pub fn prepare_for_frame(&self) {
        let cd = self.collected.borrow_mut();
        let mut fd = self.frame.borrow_mut();

        fd.prev_mouse_position = fd.mouse_position.clone();
        for n in 0 .. 5 {
            fd.mouse_up_inframe[n] = fd.mouse_pressing[n] && !cd.mouse_button[n];
            fd.mouse_down_inframe[n] = !fd.mouse_pressing[n] && cd.mouse_button[n];
            fd.mouse_pressing[n] = cd.mouse_button[n];
        }
    }

    // Mouse/Touch integrated apis
    pub fn plane_touch(&self) -> bool {
        self.frame.borrow().mouse_down_inframe[0]
    }
    pub fn plane_touching(&self) -> bool {
        self.frame.borrow().mouse_pressing[0]
    }
    pub fn plane_position(&self) -> Vector2F32 { self.frame.borrow().mouse_position.clone() }
    pub fn plane_delta_move(&self) -> Vector2F32 {
        self.frame.borrow().mouse_position.clone() - self.frame.borrow().prev_mouse_position.clone()
    }

    pub fn mouse_down(&self, knum: usize) -> bool {
        if knum >= MAX_MOUSE_BUTTONS { false } else { self.frame.borrow().mouse_down_inframe[knum] }
    }
    pub fn mouse_up(&self, knum: usize) -> bool {
        if knum >= MAX_MOUSE_BUTTONS { false } else { self.frame.borrow().mouse_up_inframe[knum] }
    }
    pub fn mouse_button(&self, knum: usize) -> bool {
        if knum >= MAX_MOUSE_BUTTONS { false } else { self.frame.borrow().mouse_pressing[knum] }
    }
    pub fn mouse_position(&self) -> Vector2F32 { self.frame.borrow().mouse_position.clone() }
    pub fn mouse_delta_move(&self) -> Vector2F32 {
        self.frame.borrow().mouse_position.clone() - self.frame.borrow().prev_mouse_position.clone()
    }
}

pub trait InputMessage: Sized {
    fn process(self, processor: &InputProcess);
}

pub enum MouseInputMessage {
    ButtonDown(usize), ButtonUp(usize), MoveAbs(Vector2F32), Wheel(Vector2F32)
}
impl InputMessage for MouseInputMessage {
    fn process(self, processor: &InputProcess) {
        match self {
            MouseInputMessage::ButtonDown(x @ 0 ... 4) => {
                processor.collected.borrow_mut().mouse_button[x] = true;
            },
            MouseInputMessage::ButtonDown(x) => trace!("MouseButton #{} Pressing", x),
            MouseInputMessage::ButtonUp(x @ 0 ... 4) => {
                processor.collected.borrow_mut().mouse_button[x] = false;
            },
            MouseInputMessage::ButtonUp(x) => trace!("MouseButton #{} Released", x),
            MouseInputMessage::MoveAbs(p) => {
                processor.collected.borrow_mut().mouse_position = p;
            },
            MouseInputMessage::Wheel(a) => {
                processor.collected.borrow_mut().mouse_wheel_motion += a;
            }
        }
    }
}
pub enum GestureInputMessage {
    Magnification(f32)
}
impl InputMessage for GestureInputMessage {
    fn process(self, processor: &InputProcess) {
        match self {
            GestureInputMessage::Magnification(mag) => {
                processor.collected.borrow_mut().gesture_magnification += mag;
            }
        }
    }
}
