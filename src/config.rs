//! External Configuration

pub const DEFAULT_CONFIG_PATH: &'static str = "data/cfg.dhall";

#[derive(serde::Deserialize)]
pub struct App {
	pub screen: Screen
}
#[derive(serde::Deserialize)]
pub struct Screen {
	pub width: usize,
	pub height: usize,
	pub style: ScreenStyle
}
#[derive(serde::Deserialize)]
pub enum ScreenStyle {
	Windowed, Unbordered, Fullscreen
}

impl App {
	pub fn load<P: AsRef<std::path::Path> + ?Sized>(path: &P) -> serde_dhall::Result<Self> {
		serde_dhall::from_file(path).parse()
	}
	pub fn load_default() -> serde_dhall::Result<Self> { Self::load(DEFAULT_CONFIG_PATH) }
}
