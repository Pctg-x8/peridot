
use std::rc::Rc;
use std::mem::replace;
use std::cell::RefCell;

const MAX_MOUSE_BUTTONS: usize = 5;
struct AsyncCollectedData
{
    mouse_motion_x: isize, mouse_motion_y: isize, mouse_wheel_motion: isize, mouse_button: [bool; MAX_MOUSE_BUTTONS]
}
#[derive(Debug)]
struct FrameData
{
    mouse_motion_x: isize, mouse_motion_y: isize, mouse_wheel_motion: isize, mouse_pressing: [bool; MAX_MOUSE_BUTTONS],
    mouse_down_inframe: [bool; MAX_MOUSE_BUTTONS], mouse_up_inframe: [bool; MAX_MOUSE_BUTTONS]
}
pub struct InputProcess
{
    collected: RefCell<AsyncCollectedData>, frame: RefCell<FrameData>
}
pub trait InputProcessPlugin
{
    fn on_start_handle(&mut self, processor: &Rc<InputProcess>);
}
impl InputProcess {
    pub fn new() -> Self
    {
        let cd = AsyncCollectedData
        {
            mouse_motion_x: 0, mouse_motion_y: 0, mouse_wheel_motion: 0, mouse_button: [false; MAX_MOUSE_BUTTONS]
        };
        let fd = FrameData
        {
            mouse_motion_x: 0, mouse_motion_y: 0, mouse_wheel_motion: 0, mouse_pressing: [false; MAX_MOUSE_BUTTONS],
            mouse_down_inframe: [false; MAX_MOUSE_BUTTONS], mouse_up_inframe: [false; MAX_MOUSE_BUTTONS]
        };
        return InputProcess { collected: cd.into(), frame: fd.into() };
    }
    pub fn dispatch_message<M: InputMessage>(&self, msg: M) { msg.process(self); }
    pub fn prepare_for_frame(&self)
    {
        let mut cd = self.collected.borrow_mut();
        let mut fd = self.frame.borrow_mut();

        fd.mouse_motion_x = replace(&mut cd.mouse_motion_x, 0);
        fd.mouse_motion_y = replace(&mut cd.mouse_motion_y, 0);
        fd.mouse_wheel_motion = replace(&mut cd.mouse_wheel_motion, 0);
        for n in 0 .. 5 {
            fd.mouse_up_inframe[n] = fd.mouse_pressing[n] && !cd.mouse_button[n];
            fd.mouse_down_inframe[n] = !fd.mouse_pressing[n] && cd.mouse_button[n];
            fd.mouse_pressing[n] = cd.mouse_button[n];
        }
    }

    // Mouse/Touch integrated apis
    pub fn plane_touch(&self) -> bool
    {
        self.frame.borrow().mouse_down_inframe[0]
    }
    pub fn plane_touching(&self) -> bool
{
        self.frame.borrow().mouse_pressing[0]
    }
    pub fn plane_delta_move(&self) -> (isize, isize)
    {
        (self.frame.borrow().mouse_motion_x, self.frame.borrow().mouse_motion_y)
    }

    pub fn mouse_down(&self, knum: usize) -> bool
    {
        if knum >= MAX_MOUSE_BUTTONS { false } else { self.frame.borrow().mouse_down_inframe[knum] }
    }
    pub fn mouse_up(&self, knum: usize) -> bool
    {
        if knum >= MAX_MOUSE_BUTTONS { false } else { self.frame.borrow().mouse_up_inframe[knum] }
    }
    pub fn mouse_button(&self, knum: usize) -> bool
    {
        if knum >= MAX_MOUSE_BUTTONS { false } else { self.frame.borrow().mouse_pressing[knum] }
    }
    pub fn mouse_delta_move(&self) -> (isize, isize)
    {
        (self.frame.borrow().mouse_motion_x, self.frame.borrow().mouse_motion_y)
    }
}

pub trait InputMessage: Sized
{
    fn process(self, processor: &InputProcess);
}

pub enum MouseInputMessage
{
    ButtonDown(usize), ButtonUp(usize), MoveRel(isize, isize), Wheel(isize)
}
impl InputMessage for MouseInputMessage
{
    fn process(self, processor: &InputProcess)
    {
        match self
        {
            MouseInputMessage::ButtonDown(x @ 0 ..= 4) =>
            {
                processor.collected.borrow_mut().mouse_button[x] = true;
            },
            MouseInputMessage::ButtonDown(x) => trace!("MouseButton #{} Pressing", x),
            MouseInputMessage::ButtonUp(x @ 0 ..= 4) =>
            {
                processor.collected.borrow_mut().mouse_button[x] = false;
            },
            MouseInputMessage::ButtonUp(x) => trace!("MouseButton #{} Released", x),
            MouseInputMessage::MoveRel(x, y) =>
            {
                processor.collected.borrow_mut().mouse_motion_x += x;
                processor.collected.borrow_mut().mouse_motion_y += y;
            },
            MouseInputMessage::Wheel(a) =>
            {
                processor.collected.borrow_mut().mouse_wheel_motion += a;
            }
        }
    }
}
