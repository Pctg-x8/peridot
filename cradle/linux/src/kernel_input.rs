
#[repr(C)]
pub struct InputEvent {
	pub time: libc::timeval,
	pub type_: u16,
	pub code: u16,
	pub value: i32
}

/// Event Types
#[repr(u16)]
pub enum EventType {
	Synchronize = 0x00,
	Key = 0x01,
	Relative = 0x02,
	Absolute = 0x03,
	Misc = 0x04,
	Switch = 0x05
}

/// Synchronization Events
#[repr(u16)]
pub enum SynchronizationEvent {
	Report = 0,
	Config = 1,
	MTReport = 2,
	Dropped = 3
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
	Misc = 0x09
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
	Misc = 0x28
}

/// Key
#[repr(u16)]
pub enum Key {
	/// = Mouse
	Left = 0x110,
	Right = 0x111,
	Middle = 0x112,
	Side = 0x113,
	Extra = 0x114,
	Forward = 0x115,
	Back = 0x116,
	Task = 0x117
}
