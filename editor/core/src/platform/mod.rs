use shared::{LogicalUnit, PixelsUnit, Size};

use crate::{
    input::{KeyboardFocusGroupRef, PerWindowKeyboardFocusState},
    persistence::WindowGeometryState,
    uicore::MountTarget,
};

#[cfg(target_os = "macos")]
pub mod mac;
#[cfg(unix)]
pub mod unix;
#[cfg(windows)]
pub mod windows;

pub trait WindowHandleBase: MountTarget {
    fn associate_extra_data<T>(&mut self, data: Box<T>);
    unsafe fn take_extra_data<T>(&mut self) -> Box<T>;
    unsafe fn extra_data_ref<'a, T>(&'a self) -> &'a T;
    unsafe fn extra_data_mut<'a, T>(&'a mut self) -> &'a mut T;

    fn keyboard_focus_state<'a>(&'a self) -> &'a PerWindowKeyboardFocusState;
    fn keyboard_focus_state_mut<'a>(&'a mut self) -> &'a mut PerWindowKeyboardFocusState;
    fn root_keyboard_focus_group(&self) -> KeyboardFocusGroupRef;

    fn close(&mut self);
    fn maximize(&self);
    fn minimize(&self);
    fn restore(&self);

    fn ui_scale_factor(&self) -> f32;
    fn client_size(&self) -> Size<LogicalUnit>;
    fn client_size_pixels(&self) -> Size<PixelsUnit>;

    fn geometry_state_snapshot(&self, syslink: &crate::SystemLink) -> WindowGeometryState;
}
