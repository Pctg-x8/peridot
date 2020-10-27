
#[repr(C)]
pub struct InputEvent {
	pub time: libc::timeval,
	pub type_: u16,
	pub code: u16,
	pub value: i32
}
