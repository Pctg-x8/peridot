use std::collections::HashMap;

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
/// Digital(Buttons) Input
pub enum NativeButtonInput {
    /// Keyboard Character(Case insensitive, cradles must pass with uppercase characters)
    Character(char),
    UpArrow,
    DownArrow,
    LeftArrow,
    RightArrow,
    Esc,
    Enter,
    Backspace,
    Space,
    LeftShift,
    RightShift,
    LeftControl,
    RightControl,
    LeftMeta,
    RightMeta,
    LeftAlt,
    RightAlt,
    CapsLock,
    Kana,
    Alphabet,
    ZenkakuHankaku,
    FunctionKey(u8),
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
    /// Touch with ID
    Touch(u32),
    POVLeft,
    POVRight,
    POVUp,
    POVDown,
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
    RightTrigger,
    /// Touch x position Move with ID
    TouchMoveX(u32),
    /// Touch y position Move with ID
    TouchMoveY(u32),
}

pub trait MappableNativeInputType {
    type ID;

    fn map_to(&self, p: &mut InputProcess, id: Self::ID);
}
impl MappableNativeInputType for NativeButtonInput {
    type ID = u16;

    fn map_to(&self, p: &mut InputProcess, id: u16) {
        p.input_map.buttonmap.insert(*self, id);
        p.input_map.max_button_id = p.input_map.max_button_id.max(id);
        if p.collected.button_pressing.len() != p.input_map.max_button_id as usize + 1 {
            p.collected
                .button_pressing
                .resize(p.input_map.max_button_id as usize + 1, false);
        }
    }
}
impl MappableNativeInputType for NativeAnalogInput {
    type ID = u8;

    fn map_to(&self, p: &mut InputProcess, id: u8) {
        p.input_map.analogmap.insert(*self, id);
        p.input_map.max_analog_id = p.input_map.max_analog_id.max(id);
        if p.collected.analog_values.len() != p.input_map.max_analog_id as usize + 1 {
            p.collected
                .analog_values
                .resize(p.input_map.max_analog_id as usize + 1, 0.0);
        }
    }
}

/// Represents key input pair as emulated as axis input
pub struct AxisKey {
    pub positive_key: NativeButtonInput,
    pub negative_key: NativeButtonInput,
}
impl MappableNativeInputType for AxisKey {
    type ID = <NativeAnalogInput as MappableNativeInputType>::ID;

    fn map_to(&self, p: &mut InputProcess, id: Self::ID) {
        p.input_map.ax_pos_buttonmap.insert(self.positive_key, id);
        p.input_map.ax_neg_buttonmap.insert(self.negative_key, id);
        p.input_map.max_analog_id = p.input_map.max_analog_id.max(id);
        if p.collected.analog_values.len() != p.input_map.max_analog_id as usize + 1 {
            p.collected
                .ax_button_pressing
                .resize(p.input_map.max_analog_id as usize + 1, (false, false));
            p.collected
                .analog_values
                .resize(p.input_map.max_analog_id as usize + 1, 0.0);
        }
    }
}

type InputMap<T> = HashMap<T, <T as MappableNativeInputType>::ID>;

pub trait NativeInput {
    fn get_pointer_position(&self, index: u32) -> Option<(f32, f32)>;

    #[allow(unused_variables)]
    fn pull(&mut self, p: NativeEventReceiver) {}
}

const MAX_MOUSE_BUTTONS: usize = 5;

struct InputMaps {
    buttonmap: InputMap<NativeButtonInput>,
    analogmap: InputMap<NativeAnalogInput>,
    ax_pos_buttonmap:
        HashMap<NativeButtonInput, <NativeAnalogInput as MappableNativeInputType>::ID>,
    ax_neg_buttonmap:
        HashMap<NativeButtonInput, <NativeAnalogInput as MappableNativeInputType>::ID>,
    max_button_id: <NativeButtonInput as MappableNativeInputType>::ID,
    max_analog_id: <NativeAnalogInput as MappableNativeInputType>::ID,
}

struct AsyncCollectedData {
    button_pressing: Vec<bool>,
    ax_button_pressing: Vec<(bool, bool)>,
    analog_values: Vec<f32>,
}

pub struct NativeEventReceiver<'s> {
    collect: &'s mut AsyncCollectedData,
    input_map: &'s InputMaps,
}
impl NativeEventReceiver<'_> {
    /// Cradle to Engine: Native Event Handler
    pub fn dispatch_button_event(&mut self, msg: NativeButtonInput, is_press: bool) {
        if let Some(&target_button_id) = self.input_map.buttonmap.get(&msg) {
            self.collect.button_pressing[target_button_id as usize] = is_press;
        }
        if let Some(&target_ax_button_id) = self.input_map.ax_pos_buttonmap.get(&msg) {
            self.collect.ax_button_pressing[target_ax_button_id as usize].0 = is_press;
        }
        if let Some(&target_ax_button_id) = self.input_map.ax_neg_buttonmap.get(&msg) {
            self.collect.ax_button_pressing[target_ax_button_id as usize].1 = is_press;
        }
    }
    /// Cradle to Engine: Native Event Handler
    pub fn dispatch_analog_event(&mut self, ty: NativeAnalogInput, value: f32, is_absolute: bool) {
        if let Some(&target_analog_id) = self.input_map.analogmap.get(&ty) {
            if is_absolute {
                self.collect.analog_values[target_analog_id as usize] = value;
            } else {
                self.collect.analog_values[target_analog_id as usize] += value;
            }
        }
    }
}

#[derive(Debug)]
struct FrameData {
    mouse_motion_x: f32,
    mouse_motion_y: f32,
    mouse_pressing: [bool; MAX_MOUSE_BUTTONS],
    mouse_down_inframe: [bool; MAX_MOUSE_BUTTONS],
    mouse_up_inframe: [bool; MAX_MOUSE_BUTTONS],
    button_press_time: Vec<std::time::Duration>,
    analog_values_abs: Vec<f32>,
}
pub struct InputProcess {
    nativelink: Option<Box<dyn NativeInput>>,
    collected: AsyncCollectedData,
    frame: FrameData,
    input_map: InputMaps,
}
impl InputProcess {
    pub fn new() -> Self {
        let cd = AsyncCollectedData {
            button_pressing: Vec::new(),
            ax_button_pressing: Vec::new(),
            analog_values: Vec::new(),
        };
        let fd = FrameData {
            mouse_motion_x: 0.0,
            mouse_motion_y: 0.0,
            mouse_pressing: [false; MAX_MOUSE_BUTTONS],
            mouse_down_inframe: [false; MAX_MOUSE_BUTTONS],
            mouse_up_inframe: [false; MAX_MOUSE_BUTTONS],
            button_press_time: Vec::new(),
            analog_values_abs: Vec::new(),
        };

        return InputProcess {
            nativelink: None,
            collected: cd,
            frame: fd,
            input_map: InputMaps {
                buttonmap: HashMap::new(),
                analogmap: HashMap::new(),
                ax_pos_buttonmap: HashMap::new(),
                ax_neg_buttonmap: HashMap::new(),
                max_button_id: 0,
                max_analog_id: 0,
            },
        };
    }
    pub fn set_nativelink(&mut self, n: Box<dyn NativeInput>) {
        self.nativelink = Some(n);
    }

    /// Cradle to Engine: Native Event Handler
    pub fn dispatch_button_event(&mut self, msg: NativeButtonInput, is_press: bool) {
        NativeEventReceiver {
            collect: &mut self.collected,
            input_map: &self.input_map,
        }
        .dispatch_button_event(msg, is_press);
    }
    /// Cradle to Engine: Native Event Handler
    pub fn dispatch_analog_event(&mut self, ty: NativeAnalogInput, value: f32, is_absolute: bool) {
        NativeEventReceiver {
            collect: &mut self.collected,
            input_map: &self.input_map,
        }
        .dispatch_analog_event(ty, value, is_absolute);
    }

    pub fn prepare_for_frame(&mut self, delta_time: std::time::Duration) {
        if let Some(ref mut p) = self.nativelink {
            p.pull(NativeEventReceiver {
                collect: &mut self.collected,
                input_map: &self.input_map,
            });
        }

        // Adjust slot size
        if self.collected.button_pressing.len() != self.frame.button_press_time.len() {
            self.frame.button_press_time.resize(
                self.collected.button_pressing.len(),
                std::time::Duration::default(),
            );
        }
        if self.collected.analog_values.len() != self.frame.analog_values_abs.len() {
            self.frame
                .analog_values_abs
                .resize(self.collected.analog_values.len(), 0.0);
        }

        for (n, &f) in self.collected.button_pressing.iter().enumerate() {
            if f {
                self.frame.button_press_time[n] += delta_time;
            } else {
                self.frame.button_press_time[n] = std::time::Duration::default();
            }
        }
        for (n, (&a, f)) in self
            .collected
            .analog_values
            .iter()
            .zip(self.collected.ax_button_pressing.iter_mut())
            .enumerate()
        {
            let (pos, neg) = *f;
            self.frame.analog_values_abs[n] =
                a + (if pos { 1.0 } else { 0.0 }) + (if neg { -1.0 } else { 0.0 });
        }
        // ax_button_pressingがない分
        if self.collected.analog_values.len() > self.collected.ax_button_pressing.len() {
            for (n, &a) in self
                .collected
                .analog_values
                .iter()
                .enumerate()
                .skip(self.collected.ax_button_pressing.len())
            {
                self.frame.analog_values_abs[n] = a;
            }
        }
        // analog_valuesがない分
        if self.collected.ax_button_pressing.len() > self.collected.analog_values.len() {
            for (n, &(pos, neg)) in self
                .collected
                .ax_button_pressing
                .iter()
                .enumerate()
                .skip(self.collected.analog_values.len())
            {
                self.frame.analog_values_abs[n] =
                    (if pos { 1.0 } else { 0.0 }) + (if neg { -1.0 } else { 0.0 });
            }
        }
    }

    /// Map native input event with id
    pub fn map<T: MappableNativeInputType>(&mut self, from: T, id: T::ID) {
        from.map_to(self, id)
    }
    /// Get button pressing time by id
    pub fn button_pressing_time(&self, id: u16) -> std::time::Duration {
        self.frame
            .button_press_time
            .get(id as usize)
            .copied()
            .unwrap_or_default()
    }
    /// Get Analog input absolute value by id
    pub fn analog_value_abs(&self, id: u8) -> f32 {
        self.frame
            .analog_values_abs
            .get(id as usize)
            .copied()
            .unwrap_or(0.0)
    }

    // Mouse/Touch integrated apis
    pub fn plane_touch(&self) -> bool {
        self.frame.mouse_down_inframe[0]
    }
    pub fn plane_touching(&self) -> bool {
        self.frame.mouse_pressing[0]
    }
    pub fn plane_delta_move(&self) -> (f32, f32) {
        (self.frame.mouse_motion_x, self.frame.mouse_motion_y)
    }

    pub fn mouse_down(&self, knum: usize) -> bool {
        if knum >= MAX_MOUSE_BUTTONS {
            false
        } else {
            self.frame.mouse_down_inframe[knum]
        }
    }
    pub fn mouse_up(&self, knum: usize) -> bool {
        if knum >= MAX_MOUSE_BUTTONS {
            false
        } else {
            self.frame.mouse_up_inframe[knum]
        }
    }
    pub fn mouse_button(&self, knum: usize) -> bool {
        if knum >= MAX_MOUSE_BUTTONS {
            false
        } else {
            self.frame.mouse_pressing[knum]
        }
    }
    pub fn mouse_delta_move(&self) -> (f32, f32) {
        (self.frame.mouse_motion_x, self.frame.mouse_motion_y)
    }

    /// Gets plane interacting position. pointer_id=0 means Generic Mouse Input
    pub fn get_plane_position(&self, pointer_id: u32) -> Option<(f32, f32)> {
        self.nativelink.as_ref()?.get_pointer_position(pointer_id)
    }
}
