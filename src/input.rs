
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
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
    Mouse(u32),
    POVLeft, POVRight, POVUp, POVDown
}
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
/// Analog(Motions) Input
pub enum NativeAnalogInput {
    MouseX,
    MouseY,
    ScrollWheel,
    /// MacBook specific: Magnification gesture
    Magnify,
    /// Stick Index(0 = Left, 1 = Right, 2.. = Other)
    StickX(u32),
    /// Stick Index(0 = Left, 1 = Right, 2.. = Other)
    StickY(u32),
    /// Stick Index(0 = Left, 1 = Right, 2.. = Other)
    StickZ(u32),
    /// Xbox controller specific
    LeftTrigger,
    /// Xbox controller specific
    RightTrigger
}

pub trait MappableNativeInputType {
    type ID;

    fn map_to(&self, p: &mut InputProcess, id: Self::ID);
}
impl MappableNativeInputType for NativeButtonInput {
    type ID = u16;

    fn map_to(&self, p: &mut InputProcess, id: u16) {
        p.buttonmap.insert(*self, id);
        p.max_button_id = p.max_button_id.max(id);
        if p.collected.borrow().button_pressing.len() != p.max_button_id as usize + 1 {
            p.collected.borrow_mut().button_pressing.resize(p.max_button_id as usize + 1, false);
        }
    }
}
impl MappableNativeInputType for NativeAnalogInput {
    type ID = u8;

    fn map_to(&self, p: &mut InputProcess, id: u8) {
        p.analogmap.insert(*self, id);
        p.max_analog_id = p.max_analog_id.max(id);
        if p.collected.borrow().analog_values.len() != p.max_analog_id as usize + 1 {
            p.collected.borrow_mut().analog_values.resize(p.max_analog_id as usize + 1, 0.0);
        }
    }
}

/// Represents key input pair as emulated as axis input
pub struct AxisKey {
    pub positive_key: NativeButtonInput,
    pub negative_key: NativeButtonInput
}
impl MappableNativeInputType for AxisKey {
    type ID = <NativeAnalogInput as MappableNativeInputType>::ID;

    fn map_to(&self, p: &mut InputProcess, id: Self::ID) {
        p.ax_pos_buttonmap.insert(self.positive_key, id);
        p.ax_neg_buttonmap.insert(self.negative_key, id);
        p.max_analog_id = p.max_analog_id.max(id);
        if p.collected.borrow().analog_values.len() != p.max_analog_id as usize + 1 {
            p.collected.borrow_mut().ax_button_pressing.resize(p.max_analog_id as usize + 1, (false, false));
            p.collected.borrow_mut().analog_values.resize(p.max_analog_id as usize + 1, 0.0);
        }
    }
}

type InputMap<T> = HashMap<T, <T as MappableNativeInputType>::ID>;

pub trait NativeInput {
    fn get_pointer_position(&self, index: u32) -> Option<(f32, f32)>;
}

const MAX_MOUSE_BUTTONS: usize = 5;
struct AsyncCollectedData {
    button_pressing: Vec<bool>,
    ax_button_pressing: Vec<(bool, bool)>,
    analog_values: Vec<f32>
}
#[derive(Debug)]
struct FrameData {
    mouse_motion_x: f32, mouse_motion_y: f32, mouse_wheel_motion: f32, mouse_pressing: [bool; MAX_MOUSE_BUTTONS],
    mouse_down_inframe: [bool; MAX_MOUSE_BUTTONS], mouse_up_inframe: [bool; MAX_MOUSE_BUTTONS],
    button_press_time: Vec<std::time::Duration>,
    analog_values_abs: Vec<f32>
}
pub struct InputProcess {
    nativelink: Option<Box<dyn NativeInput>>,
    collected: RefCell<AsyncCollectedData>, frame: RefCell<FrameData>,
    buttonmap: InputMap<NativeButtonInput>,
    analogmap: InputMap<NativeAnalogInput>,
    ax_pos_buttonmap: HashMap<NativeButtonInput, <NativeAnalogInput as MappableNativeInputType>::ID>,
    ax_neg_buttonmap: HashMap<NativeButtonInput, <NativeAnalogInput as MappableNativeInputType>::ID>,
    max_button_id: <NativeButtonInput as MappableNativeInputType>::ID,
    max_analog_id: <NativeAnalogInput as MappableNativeInputType>::ID
}
impl InputProcess {
    pub fn new() -> Self {
        let cd = AsyncCollectedData {
            button_pressing: Vec::new(),
            ax_button_pressing: Vec::new(),
            analog_values: Vec::new()
        };
        let fd = FrameData {
            mouse_motion_x: 0.0, mouse_motion_y: 0.0, mouse_wheel_motion: 0.0,
            mouse_pressing: [false; MAX_MOUSE_BUTTONS],
            mouse_down_inframe: [false; MAX_MOUSE_BUTTONS], mouse_up_inframe: [false; MAX_MOUSE_BUTTONS],
            button_press_time: Vec::new(),
            analog_values_abs: Vec::new()
        };

        return InputProcess {
            nativelink: None,
            collected: cd.into(), frame: fd.into(),
            buttonmap: HashMap::new(),
            analogmap: HashMap::new(),
            ax_pos_buttonmap: HashMap::new(),
            ax_neg_buttonmap: HashMap::new(),
            max_button_id: 0,
            max_analog_id: 0
        };
    }
    pub fn set_nativelink(&mut self, n: Box<dyn NativeInput>) {
        self.nativelink = Some(n);
    }

    /// Cradle to Engine: Native Event Handler
    pub fn dispatch_button_event(&self, msg: NativeButtonInput, is_press: bool) {
        if let Some(&target_button_id) = self.buttonmap.get(&msg) {
            self.collected.borrow_mut().button_pressing[target_button_id as usize] |= is_press;
        }
        if let Some(&target_ax_button_id) = self.ax_pos_buttonmap.get(&msg) {
            self.collected.borrow_mut().ax_button_pressing[target_ax_button_id as usize].0 |= is_press;
        }
        if let Some(&target_ax_button_id) = self.ax_neg_buttonmap.get(&msg) {
            self.collected.borrow_mut().ax_button_pressing[target_ax_button_id as usize].1 |= is_press;
        }
    }
    /// Cradle to Engine: Native Event Handler
    pub fn dispatch_analog_event(&self, ty: NativeAnalogInput, value: f32, is_absolute: bool) {
        if let Some(&target_analog_id) = self.analogmap.get(&ty) {
            if is_absolute {
                self.collected.borrow_mut().analog_values[target_analog_id as usize] = value;
            } else {
                self.collected.borrow_mut().analog_values[target_analog_id as usize] += value;
            }
        }
    }
    
    pub fn prepare_for_frame(&self, delta_time: std::time::Duration) {
        let mut cd = self.collected.borrow_mut();
        let mut fd = self.frame.borrow_mut();

        // Adjust slot size
        if cd.button_pressing.len() != fd.button_press_time.len() {
            fd.button_press_time.resize(cd.button_pressing.len(), std::time::Duration::default());
        }
        if cd.analog_values.len() != fd.analog_values_abs.len() {
            fd.analog_values_abs.resize(cd.analog_values.len(), 0.0);
        }

        for (n, f) in cd.button_pressing.iter_mut().enumerate() {
            if *f {
                fd.button_press_time[n] += delta_time;
            } else {
                fd.button_press_time[n] = std::time::Duration::default();
            }
            *f = false;
        }
        let &mut AsyncCollectedData { ref analog_values, ref mut ax_button_pressing, .. } = &mut *cd;
        for (n, (&a, f)) in analog_values.iter().zip(ax_button_pressing).enumerate() {
            let (pos, neg) = *f;
            fd.analog_values_abs[n] = a + (if pos { 1.0 } else { 0.0 }) + (if neg { -1.0 } else { 0.0 });
            *f = (false, false);
        }
    }

    /// Map native input event with id
    pub fn map<T: MappableNativeInputType>(&mut self, from: T, id: T::ID) {
        from.map_to(self, id)
    }
    /// Get button pressing time by id
    pub fn button_pressing_time(&self, id: u16) -> std::time::Duration {
        self.frame.borrow().button_press_time.get(id as usize).copied().unwrap_or_default()
    }
    /// Get Analog input absolute value by id
    pub fn analog_value_abs(&self, id: u8) -> f32 {
        self.frame.borrow().analog_values_abs.get(id as usize).copied().unwrap_or(0.0)
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
    pub fn plane_delta_move(&self) -> (f32, f32)
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
    pub fn mouse_delta_move(&self) -> (f32, f32)
    {
        (self.frame.borrow().mouse_motion_x, self.frame.borrow().mouse_motion_y)
    }

    /// Gets plane interacting position. pointer_id=0 means Generic Mouse Input
    pub fn get_plane_position(&self, pointer_id: u32) -> Option<(f32, f32)> {
        self.nativelink.as_ref()?.get_pointer_position(pointer_id)
    }
}
