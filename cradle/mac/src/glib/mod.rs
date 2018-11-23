
use peridot::{AssetLoader, PlatformRenderTarget};
use std::marker::PhantomData;

pub struct Game<AL: AssetLoader, PRT: PlatformRenderTarget> {
    _p: PhantomData<(*const AL, *const PRT)>
}
impl<AL: AssetLoader, PRT: PlatformRenderTarget> Game<AL, PRT> {
    pub const NAME: &'static str = "example-mmdloader";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<AL: AssetLoader, PRT: PlatformRenderTarget> peridot::EngineEvents<AL, PRT> for Game<AL, PRT> {
    fn init(e: &peridot::Engine<Self, AL, PRT>) -> Self {
        println!("Engine Initialized!!");
        Game { _p: PhantomData }
    }
}
