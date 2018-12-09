
use std::marker::PhantomData;
pub struct Game<AL: peridot::PlatformAssetLoader, PRT: peridot::PlatformRenderTarget>(PhantomData<(*const AL, *const PRT)>);
impl<AL: peridot::PlatformAssetLoader, PRT: peridot::PlatformRenderTarget> Game<AL, PRT> {
	pub const NAME: &'static str = "Peridot Example: Empty";
	pub const VERSION: (u32, u32, u32) = (1, 0, 0);
}

impl<AL: peridot::PlatformAssetLoader, PRT: peridot::PlatformRenderTarget> peridot::EngineEvents<AL, PRT> for Game<AL, PRT> {
	fn init(_e: &peridot::Engine<Self, AL, PRT>) -> Self { Game(PhantomData) }
}
