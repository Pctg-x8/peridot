
use std::rc::Rc;
use std::mem::replace;
use std::cell::RefCell;

/// Digital(Buttons) Input
pub enum NativeButtonInput {
    /// Keyboard Character(Case insensitive, cradles must pass with uppercase characters)
    Character(char),
    UpArrow, DownArrow, LeftArrow, RightArrow,
    Esc, Enter, Backspace, Space, LeftShift, RightShift, LeftControl, RightControl, LeftMeta, RightMeta,
    LeftAlt, RightAlt, CapsLock, Kana, Alphabet, ZenkakuHankaku, FunctionKey(u8),
    /// DUALSHOCK equivalent is ○
    ButtonA,
    /// DUALSHOCK equivalent is ×
    ButtonB,
    ButtonC,
    ButtonD,
    /// DUALSHOCK equivalent is △
    ButtonX,
    /// DUALSHOCK equivalent is □
    ButtonY,
    ButtonStart,
    ButtonSelect,
    ButtonMeta,
    /// DUALSHOCK equivalent is L1(L2 will be simulated as NativeAnalogInput::LeftTrigger(1.0))
    ButtonL,
    /// DUALSHOCK equivalent is R1(R2 will be simulated as NativeAnalogInput::RightTrigger(1.0))
    ButtonR,
    /// Stick Index
    Stick(u32),
    /// Mouse Button(0 = Left, 1 = Right, 2 = Center, 3.. = Other)
    Mouse(u32)
}
/// Analog(Motions) Input
pub enum NativeAnalogInput {
    /// Relative Input
    MouseX(i32),
    /// Relative Input
    MouseY(i32),
    /// Relative Input
    ScrollWheel(i32),
    /// Relative Input
    Magnify(f32),
    /// Stick Index(0 = Left, 1 = Right, 2.. = Other), Absolute X Amount, Absolute Y Amount
    Stick(u32, f32, f32),
    /// Xbox controller specific: Relative Input
    LeftTrigger(f32),
    /// Xbox controller specific: Relative Input
    RightTrigger(f32)
}

const MAX_MOUSE_BUTTONS: usize = 5;
struct AsyncCollectedData
{
    mouse_motion_x: i32, mouse_motion_y: i32, mouse_wheel_motion: i32, mouse_button: [bool; MAX_MOUSE_BUTTONS]
}
#[derive(Debug)]
struct FrameData
{
    mouse_motion_x: i32, mouse_motion_y: i32, mouse_wheel_motion: i32, mouse_pressing: [bool; MAX_MOUSE_BUTTONS],
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
impl InputProcess
{
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
    pub fn prepare_for_frame(&self)
    {
        let mut cd = self.collected.borrow_mut();
        let mut fd = self.frame.borrow_mut();

        fd.mouse_motion_x = replace(&mut cd.mouse_motion_x, 0);
        fd.mouse_motion_y = replace(&mut cd.mouse_motion_y, 0);
        fd.mouse_wheel_motion = replace(&mut cd.mouse_wheel_motion, 0);
        for n in 0 .. 5
        {
            fd.mouse_up_inframe[n] = fd.mouse_pressing[n] && !cd.mouse_button[n];
            fd.mouse_down_inframe[n] = !fd.mouse_pressing[n] && cd.mouse_button[n];
            fd.mouse_pressing[n] = cd.mouse_button[n];
        }
    }

    /// Cradle to Engine Event Handler
    pub fn dispatch_button_event(&self, msg: NativeButtonInput, is_press: bool) {
        match msg {
            NativeButtonInput::Mouse(b) if (0 .. MAX_MOUSE_BUTTONS as u32).contains(&b) => {
                self.collected.borrow_mut().mouse_button[b as usize] = is_press;
            },
            _ => (/* nop */)
        }
    }
    /// Cradle to Engine Event Handler
    pub fn dispatch_analog_event(&self, msg: NativeAnalogInput) {
        match msg {
            NativeAnalogInput::MouseX(r) => { self.collected.borrow_mut().mouse_motion_x += r; }
            NativeAnalogInput::MouseY(r) => { self.collected.borrow_mut().mouse_motion_y += r; }
            NativeAnalogInput::ScrollWheel(s) => { self.collected.borrow_mut().mouse_wheel_motion += s; },
            _ => (/* nop */)
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
    pub fn plane_delta_move(&self) -> (i32, i32)
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
    pub fn mouse_delta_move(&self) -> (i32, i32)
    {
        (self.frame.borrow().mouse_motion_x, self.frame.borrow().mouse_motion_y)
    }
}
