use std::{collections::HashMap, sync::Arc};

use parking_lot::RwLock;

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

    fn map_to(&self, st: &mut InputProcessSharedState, id: Self::ID);
}
impl MappableNativeInputType for NativeButtonInput {
    type ID = u16;

    fn map_to(&self, st: &mut InputProcessSharedState, id: u16) {
        st.input_map.buttonmap.insert(*self, id);
        st.input_map.max_button_id = st.input_map.max_button_id.max(id);
        if st.collected.button_pressing.len() != st.input_map.max_button_id as usize + 1 {
            let new_slot_count = st.input_map.max_analog_id as usize + 1;

            st.collected.button_pressing.resize(new_slot_count, false);
        }
    }
}
impl MappableNativeInputType for NativeAnalogInput {
    type ID = u8;

    fn map_to(&self, st: &mut InputProcessSharedState, id: u8) {
        st.input_map.analogmap.insert(*self, id);
        st.input_map.max_analog_id = st.input_map.max_analog_id.max(id);
        if st.collected.analog_values.len() != st.input_map.max_analog_id as usize + 1 {
            let new_slot_count = st.input_map.max_analog_id as usize + 1;

            st.collected.analog_values.resize(new_slot_count, 0.0);
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

    fn map_to(&self, st: &mut InputProcessSharedState, id: Self::ID) {
        st.input_map.ax_pos_buttonmap.insert(self.positive_key, id);
        st.input_map.ax_neg_buttonmap.insert(self.negative_key, id);
        st.input_map.max_analog_id = st.input_map.max_analog_id.max(id);
        if st.collected.analog_values.len() != st.input_map.max_analog_id as usize + 1 {
            let new_slot_count = st.input_map.max_analog_id as usize + 1;

            st.collected
                .ax_button_pressing
                .resize(new_slot_count, (false, false));
            st.collected.analog_values.resize(new_slot_count, 0.0);
        }
    }
}

type InputMap<T> = HashMap<T, <T as MappableNativeInputType>::ID>;

pub trait NativeInput {
    fn get_pointer_position(&self, index: u32) -> Option<(f32, f32)>;

    #[allow(unused_variables)]
    fn pull(&mut self, p: NativeEventReceiver) {}
}

pub struct NativeEventReceiver<'s> {
    collect_data: &'s mut AsyncCollectedData,
    input_map: &'s InputMaps,
}
impl<'s> NativeEventReceiver<'s> {
    /// Cradle to Engine: Native Event Handler
    pub fn dispatch_button_event(&mut self, msg: NativeButtonInput, is_press: bool) {
        if let Some(&target_button_id) = self.input_map.buttonmap.get(&msg) {
            self.collect_data.button_pressing[target_button_id as usize] = is_press;
        }
        if let Some(&target_ax_button_id) = self.input_map.ax_pos_buttonmap.get(&msg) {
            self.collect_data.ax_button_pressing[target_ax_button_id as usize].0 = is_press;
        }
        if let Some(&target_ax_button_id) = self.input_map.ax_neg_buttonmap.get(&msg) {
            self.collect_data.ax_button_pressing[target_ax_button_id as usize].1 = is_press;
        }
    }

    /// Cradle to Engine: Native Event Handler
    pub fn dispatch_analog_event(&mut self, ty: NativeAnalogInput, value: f32, is_absolute: bool) {
        if let Some(&target_analog_id) = self.input_map.analogmap.get(&ty) {
            if is_absolute {
                self.collect_data.analog_values[target_analog_id as usize] = value;
            } else {
                self.collect_data.analog_values[target_analog_id as usize] += value;
            }
        }
    }
}

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
impl InputMaps {
    pub fn new() -> Self {
        Self {
            buttonmap: HashMap::new(),
            analogmap: HashMap::new(),
            ax_pos_buttonmap: HashMap::new(),
            ax_neg_buttonmap: HashMap::new(),
            max_button_id: 0,
            max_analog_id: 0,
        }
    }
}

struct AsyncCollectedData {
    button_pressing: Vec<bool>,
    ax_button_pressing: Vec<(bool, bool)>,
    analog_values: Vec<f32>,
}
#[derive(Debug)]
struct FrameData {
    button_press_time: Vec<std::time::Duration>,
    analog_values_abs: Vec<f32>,
}

pub struct InputProcessSharedState {
    nativelink: Option<Box<dyn NativeInput>>,
    collected: AsyncCollectedData,
    frame: FrameData,
    input_map: InputMaps,
}
impl InputProcessSharedState {
    #[inline]
    pub fn set_nativelink(&mut self, n: Box<dyn NativeInput>) {
        self.nativelink = Some(n);
    }

    #[inline]
    pub fn make_event_receiver<'s>(&'s mut self) -> NativeEventReceiver<'s> {
        NativeEventReceiver {
            collect_data: &mut self.collected,
            input_map: &self.input_map,
        }
    }

    /// Cradle to Engine: Native Event Handler
    #[inline]
    fn dispatch_button_event(&mut self, msg: NativeButtonInput, is_press: bool) {
        self.make_event_receiver()
            .dispatch_button_event(msg, is_press)
    }

    /// Cradle to Engine: Native Event Handler
    #[inline]
    fn dispatch_analog_event(&mut self, ty: NativeAnalogInput, value: f32, is_absolute: bool) {
        self.make_event_receiver()
            .dispatch_analog_event(ty, value, is_absolute)
    }

    pub(crate) fn prepare_for_frame(&mut self, delta_time: std::time::Duration) {
        if let Some(ref mut p) = self.nativelink {
            p.pull(NativeEventReceiver {
                collect_data: &mut self.collected,
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

        let analog_values = self.collected.analog_values.iter().copied().chain(
            std::iter::repeat(0.0).take(
                self.collected
                    .ax_button_pressing
                    .len()
                    .saturating_sub(self.collected.analog_values.len()),
            ),
        );
        let emulated_analog_values = self.collected.ax_button_pressing.iter().chain(
            std::iter::repeat(&(false, false)).take(
                self.collected
                    .analog_values
                    .len()
                    .saturating_sub(self.collected.ax_button_pressing.len()),
            ),
        );
        for (n, (a, &(pos, neg))) in analog_values.zip(emulated_analog_values).enumerate() {
            self.frame.analog_values_abs[n] =
                a + (if pos { 1.0 } else { 0.0 }) + (if neg { -1.0 } else { 0.0 });
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

    /// Gets plane interacting position. pointer_id=0 means Generic Mouse Input
    pub fn get_plane_position(&self, pointer_id: u32) -> Option<(f32, f32)> {
        self.nativelink.as_ref()?.get_pointer_position(pointer_id)
    }
}

#[derive(Clone)]
pub struct InputProcess {
    state: Arc<RwLock<InputProcessSharedState>>,
}
impl InputProcess {
    pub fn new() -> Self {
        let cd = AsyncCollectedData {
            button_pressing: Vec::new(),
            ax_button_pressing: Vec::new(),
            analog_values: Vec::new(),
        };
        let fd = FrameData {
            button_press_time: Vec::new(),
            analog_values_abs: Vec::new(),
        };

        InputProcess {
            state: Arc::new(RwLock::new(InputProcessSharedState {
                nativelink: None,
                collected: cd,
                frame: fd,
                input_map: InputMaps::new(),
            })),
        }
    }

    #[inline]
    pub fn state_read_lock<'s>(
        &'s self,
    ) -> parking_lot::RwLockReadGuard<'s, InputProcessSharedState> {
        self.state.read()
    }

    #[inline]
    pub fn state_write_lock<'s>(
        &'s self,
    ) -> parking_lot::RwLockWriteGuard<'s, InputProcessSharedState> {
        self.state.write()
    }

    #[inline]
    pub fn set_nativelink(&self, n: Box<dyn NativeInput>) {
        self.state_write_lock().set_nativelink(n);
    }

    /// Cradle to Engine: Native Event Handler
    #[inline]
    pub fn dispatch_button_event(&self, msg: NativeButtonInput, is_press: bool) {
        self.state_write_lock().dispatch_button_event(msg, is_press);
    }

    /// Cradle to Engine: Native Event Handler
    #[inline]
    pub fn dispatch_analog_event(&self, ty: NativeAnalogInput, value: f32, is_absolute: bool) {
        self.state_write_lock()
            .dispatch_analog_event(ty, value, is_absolute);
    }

    pub(crate) fn prepare_for_frame(&self, delta_time: std::time::Duration) {
        self.state.write().prepare_for_frame(delta_time);
    }

    /// Map native input event with id
    pub fn map<T: MappableNativeInputType>(&mut self, from: T, id: T::ID) {
        from.map_to(&mut self.state_write_lock(), id)
    }

    /// Get button pressing time by id
    #[inline]
    pub fn button_pressing_time(&self, id: u16) -> std::time::Duration {
        self.state_read_lock().button_pressing_time(id)
    }
    /// Get Analog input absolute value by id
    #[inline]
    pub fn analog_value_abs(&self, id: u8) -> f32 {
        self.state_read_lock().analog_value_abs(id)
    }

    /// Gets plane interacting position. pointer_id=0 means Generic Mouse Input
    #[inline]
    pub fn get_plane_position(&self, pointer_id: u32) -> Option<(f32, f32)> {
        self.state_read_lock().get_plane_position(pointer_id)
    }
}
